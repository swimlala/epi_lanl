CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:17Z UW 3.1 conversion   
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  78   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7x   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8<   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8\   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~       axis      T           8`   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8h   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~            8l   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8t   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8|   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  A8   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  C    PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  T8   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  [�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  ]�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  eP   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  g8   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  n�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  vh   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  xP   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �h   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �$   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �(   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        �8   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        �<   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �@   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �DArgo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               _A   AO  20111130141321  20190522121826  1727_5046_095                   2C  D   APEX                            2143                            040306                          846 @Ԑr�l@	1   @Ԑsm�?�@6�=p��
�c�I�^51   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   BffB  BffB   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�ffB�33B���B�  B�  B�  B�  B���B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C	�fC  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;fD;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DQ��DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY�fDZ  DZ� D[  D[� D\  D\� D]fD]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Ddy�De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� DkfDk�fDl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Dsy�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   BffB  BffB   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�ffB�33B���B�  B�  B�  B�  B���B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C	�fC  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;fD;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DQ��DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY�fDZ  DZ� D[  D[� D\  D\� D]fD]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Ddy�De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� DkfDk�fDl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Dsy�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�^5A�^5A�Q�A�M�A�O�A�M�A�G�A�E�A�C�A�A�A�=qA�(�A��mA���Aҟ�A���A�t�A�S�A�C�A��A�JA���A���A��A��A���A���A��TA���A���A���A�ƨAЉ7Aͣ�A�K�A�hsAɧ�A�9XA�^5AƗ�A�9XA��A�bNAĉ7A���A���AÛ�A��A�A��/A��A���A��9A��A��A��/A���A��jA��A��HA���A��A��HA��uA�VA��-A���A�G�A��^A�VA�bA��^A��mA�/A��;A�p�A�XA���A�bA��A�bA���A��\A��A�&�A���A��DA��^A��A���A��!A��!A�-A��FA� �A��RA��+A�ZA��A�O�A�l�A�I�A��A�z�A�%A��A��/A�ffA��TA�G�A�{A�=qA��A�+A���A���A~��A}�A|~�A{��Azn�Ay�AwƨAs�Aq��Am/Aj��Ai��Ai�Ai�AgAc��Ab1A`�!A^�DA]AZ�AYt�AX�+AVn�AT��AT �AR�AR�AQ�TAP�AO
=AN�+AM&�AL{AJbNAI�AH�9AG?}AE�7AC�ABVA@�A?��A?
=A>�jA?33A=�
A;?}A:�A9XA8��A7ƨA6�A5;dA4��A4ȴA3��A2��A1�A1`BA0�A/�^A/t�A.�9A.=qA,�9A+�A)��A(��A(��A( �A&�A&��A&��A&�+A%��A%?}A$��A#l�A"��A!�hA�#AO�A�
A�AA�AdZA{A��A��A�A�yAZA&�AbNA�AG�A{At�A�\AE�A��A��Ap�A��A
r�A	��A�9AG�A��A��AƨA�RAn�AI�A{A�A�^A��A �@���@��D@���@�I�@��u@�F@�@��H@��y@�Q�@�S�@��@��@�z�@��@柾@��@�@�w@�K�@�M�@�bN@�S�@�@���@�ƨ@ف@�ƨ@Չ7@�|�@�X@�r�@�b@�33@���@�ȴ@�v�@���@� �@˶F@��@�J@��/@�b@ǥ�@���@�x�@���@�I�@�1@��;@�K�@°!@��#@��@�bN@���@�C�@��\@��@��@��@��;@���@��P@���@���@�E�@���@�=q@�M�@�M�@���@�X@��@��D@�(�@��P@���@���@���@��/@�r�@�
=@��+@�^5@�dZ@�/@��h@��@�J@���@���@��h@�G�@��@��@��@���@���@�v�@�V@��-@�K�@���@��!@��!@�@�I�@��y@�v�@��-@�C�@�{@�1'@�l�@���@�ff@�{@���@�%@�A�@��w@��P@�\)@��@�V@�@���@�x�@�&�@�%@���@��@�j@�1@��@�S�@�+@�@��!@���@�J@���@���@��@��@���@�X@�Ĝ@���@�j@��@�\)@�K�@���@���@�t�@��y@�-@���@��7@�x�@�O�@��D@�(�@���@��@���@�K�@�n�@�x�@�O�@�G�@��@���@�I�@�b@��w@�t�@��P@�bN@�%@�x�@�x�@�7L@���@���@�p�@�hs@��@�%@��/@��@��u@��u@�z�@�bN@�A�@�b@�  @���@���@��@��;@�@���@���@�V@��@���@�&�@�/@��^@�{@�{@��T@��@��u@��@��u@��@��`@���@�r�@�bN@�Z@�A�@�(�@�1@��P@�\)@�dZ@�K�@�C�@�33@�"�@��@���@���@�ff@�J@���@��@�?}@�/@�V@��`@���@�Q�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�^5A�^5A�Q�A�M�A�O�A�M�A�G�A�E�A�C�A�A�A�=qA�(�A��mA���Aҟ�A���A�t�A�S�A�C�A��A�JA���A���A��A��A���A���A��TA���A���A���A�ƨAЉ7Aͣ�A�K�A�hsAɧ�A�9XA�^5AƗ�A�9XA��A�bNAĉ7A���A���AÛ�A��A�A��/A��A���A��9A��A��A��/A���A��jA��A��HA���A��A��HA��uA�VA��-A���A�G�A��^A�VA�bA��^A��mA�/A��;A�p�A�XA���A�bA��A�bA���A��\A��A�&�A���A��DA��^A��A���A��!A��!A�-A��FA� �A��RA��+A�ZA��A�O�A�l�A�I�A��A�z�A�%A��A��/A�ffA��TA�G�A�{A�=qA��A�+A���A���A~��A}�A|~�A{��Azn�Ay�AwƨAs�Aq��Am/Aj��Ai��Ai�Ai�AgAc��Ab1A`�!A^�DA]AZ�AYt�AX�+AVn�AT��AT �AR�AR�AQ�TAP�AO
=AN�+AM&�AL{AJbNAI�AH�9AG?}AE�7AC�ABVA@�A?��A?
=A>�jA?33A=�
A;?}A:�A9XA8��A7ƨA6�A5;dA4��A4ȴA3��A2��A1�A1`BA0�A/�^A/t�A.�9A.=qA,�9A+�A)��A(��A(��A( �A&�A&��A&��A&�+A%��A%?}A$��A#l�A"��A!�hA�#AO�A�
A�AA�AdZA{A��A��A�A�yAZA&�AbNA�AG�A{At�A�\AE�A��A��Ap�A��A
r�A	��A�9AG�A��A��AƨA�RAn�AI�A{A�A�^A��A �@���@��D@���@�I�@��u@�F@�@��H@��y@�Q�@�S�@��@��@�z�@��@柾@��@�@�w@�K�@�M�@�bN@�S�@�@���@�ƨ@ف@�ƨ@Չ7@�|�@�X@�r�@�b@�33@���@�ȴ@�v�@���@� �@˶F@��@�J@��/@�b@ǥ�@���@�x�@���@�I�@�1@��;@�K�@°!@��#@��@�bN@���@�C�@��\@��@��@��@��;@���@��P@���@���@�E�@���@�=q@�M�@�M�@���@�X@��@��D@�(�@��P@���@���@���@��/@�r�@�
=@��+@�^5@�dZ@�/@��h@��@�J@���@���@��h@�G�@��@��@��@���@���@�v�@�V@��-@�K�@���@��!@��!@�@�I�@��y@�v�@��-@�C�@�{@�1'@�l�@���@�ff@�{@���@�%@�A�@��w@��P@�\)@��@�V@�@���@�x�@�&�@�%@���@��@�j@�1@��@�S�@�+@�@��!@���@�J@���@���@��@��@���@�X@�Ĝ@���@�j@��@�\)@�K�@���@���@�t�@��y@�-@���@��7@�x�@�O�@��D@�(�@���@��@���@�K�@�n�@�x�@�O�@�G�@��@���@�I�@�b@��w@�t�@��P@�bN@�%@�x�@�x�@�7L@���@���@�p�@�hs@��@�%@��/@��@��u@��u@�z�@�bN@�A�@�b@�  @���@���@��@��;@�@���@���@�V@��@���@�&�@�/@��^@�{@�{@��T@��@��u@��@��u@��@��`@���@�r�@�bN@�Z@�A�@�(�@�1@��P@�\)@�dZ@�K�@�C�@�33@�"�@��@���@���@�ff@�J@���@��@�?}@�/@�V@��`@���@�Q�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB@�B@�BA�BA�BA�BB�BB�BB�BC�BB�BA�B@�B;dB8RB33B#�B�B�B�B�B�B�B�B�B!�B$�B$�B#�B"�B#�B$�B$�B$�B}�B��B��B��B�oB�%Bs�Bm�Bs�Br�B|�B�+B�DB�=Bz�BD�B33B-B.B>wBffBx�B�B�uB��B��B��B�B�!B�B�B�B��B��B��B��B��B��B��B�hB�JB�7B�Bu�Bo�BhsB]/BP�B?}B,B�B�B+B�B�
BƨB�LB��B�\B�+B�Bx�Bq�Bl�BgmB\)BG�B$�B	7BB
��B
�mB
�/B
�B
�
B
��B
ÖB
�9B
��B
��B
�B
s�B
e`B
Q�B
J�B
D�B
?}B
9XB
33B
$�B
\B
B	�B	�yB	�mB	�mB	�ZB	��B	�qB	�LB	�3B	��B	��B	�bB	�%B	�B	p�B	]/B	T�B	N�B	R�B	[#B	R�B	F�B	Q�B	O�B	H�B	?}B	7LB	1'B	'�B	 �B	bB	+B��B��B��B��B	
=B	1B��B��B��B��B�B�B�sB�mB�fB�HB�B�
B�B��B��B��B��B��BŢB��B�dB�XB�LB�?B�-B�-B�'B�!B�B�B��B��B��B��B��B�uB�PB�7B�B�B{�By�Bu�Bt�Br�Bp�Bq�Bp�Bo�Bl�BjBiyBiyBk�BiyBdZB`BB`BB_;B\)BYBW
BS�BQ�BO�BO�BO�BN�BN�BM�BM�BK�BI�BF�BD�BD�BA�BD�BE�BD�BA�BC�BM�BL�BL�BK�BI�BH�BG�BE�BC�BB�BA�B?}B>wB>wB?}B=qB6FB1'B.B-B49B7LB49B49B49B49B49B33B6FB7LB7LB7LB<jB:^B8RB8RB:^B:^B:^B;dB;dB;dB:^B9XB9XB9XB8RB8RB8RB9XB:^B=qBA�BJ�BK�BK�BL�BL�BL�BN�BP�BQ�BQ�BP�BP�BO�BN�BP�BT�BZB[#BYBZB]/B`BBdZBiyBv�B�%B�1B�DB�hB��B��B��B��B��B�RB��BĜBƨBǮB��B��BɺBƨBȴBɺBƨBBĜBɺBǮB��B�wBÖBƨBȴB��B��B��B�B�)B�;B�BB�HB�TB�fB�B�B�B�B�B��B��B��B	  B	B	1B	
=B	PB	�B	�B	�B	�B	�B	�B	 �B	"�B	&�B	)�B	+B	,B	-B	/B	33B	8RB	9XB	<jB	@�B	D�B	E�B	I�B	I�B	J�B	M�B	P�B	R�B	S�B	S�B	T�B	VB	XB	YB	YB	ZB	^5B	aHB	bNB	e`B	jB	m�B	s�B	y�B	~�B	�B	�B	�%B	�+B	�+B	�=B	�=B	�JB	�PB	�\B	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	�B	�B	�B	�B	�!B	�-B	�9B	�?B	�?B	�FB	�LB	�LB	�RB	�^B	�dB	�jB	�wB	�}B	��B	B	ĜB	ĜB	ƨB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B@�B@�BA�BA�BA�BB�BB�BB�BC�BB�BB�BB�B<jB:^B8RB%�B �B�B�B�B�B�B�B�B!�B$�B$�B#�B"�B#�B$�B%�B&�B�B��B��B��B��B�PBu�Bn�Bv�Bv�B� B�1B�JB�uB�7BJ�B7LB2-B1'BC�BiyBy�B�B��B��B�B�-B�-B�'B�'B�!B�B�B��B��B��B��B��B��B��B�VB�PB�bBz�Bs�Bm�BcTBZBF�B33B!�B�BPB��B�)B��B��B��B�oB�=B�Bz�Br�Bm�Bk�BffBS�B1'BJB%BB
�B
�;B
�/B
�B
��B
ɺB
�RB
�B
��B
�B
w�B
k�B
T�B
L�B
F�B
B�B
;dB
8RB
1'B
{B
\B	��B	�B	�mB	�yB	�B	�BB	ÖB	�jB	�^B	�B	��B	��B	�=B	�7B	v�B	_;B	ZB	P�B	S�B	`BB	XB	H�B	W
B	S�B	N�B	B�B	:^B	6FB	.B	&�B	uB	
=B	B��B��B��B	VB	\B	B��B��B��B��B�B�yB�sB�yB�`B�/B�B�B�B��B��B��B��B��BĜB�qB�^B�XB�XB�3B�3B�-B�-B�B�B�B��B��B��B��B��B�bB�DB�1B�B}�B|�Bz�Bv�Bt�Bt�Bt�Bq�Br�Bp�Bl�Bk�BjBl�Bl�BhsBe`BcTBaHB_;B]/B[#BW
BT�BR�BP�BP�BO�BO�BN�BN�BN�BL�BK�BH�BG�BG�BF�BF�BF�BH�BH�BO�BO�BM�BN�BL�BI�BI�BG�BE�BC�BC�BB�B@�B?}BA�B@�B9XB33B1'B0!B7LB8RB49B5?B5?B49B5?B5?B7LB8RB8RB9XB>wB;dB9XB;dB;dB;dB;dB<jB;dB<jB;dB:^B;dB:^B9XB9XB9XB;dB;dB=qBC�BK�BK�BK�BL�BN�BQ�BO�BP�BQ�BR�BQ�BQ�BP�BO�BP�BT�B[#B]/B[#B[#B_;BaHBdZBgmBs�B�B�1B�DB�oB��B��B��B��B��B�RBBŢBǮBǮB��B��B��BǮBȴB��BȴBĜBŢB��B��BÖB��BĜBǮBɺB��B��B��B�B�/B�;B�BB�NB�ZB�mB�B�B�B�B�B��B��B��B	B	B	1B	
=B	VB	�B	�B	�B	�B	�B	�B	 �B	"�B	'�B	)�B	,B	-B	.B	/B	33B	8RB	9XB	=qB	A�B	E�B	E�B	I�B	I�B	K�B	N�B	Q�B	R�B	S�B	T�B	VB	W
B	XB	YB	YB	[#B	_;B	aHB	cTB	ffB	jB	l�B	r�B	y�B	~�B	�B	�B	�%B	�+B	�+B	�DB	�=B	�JB	�PB	�\B	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	�B	�B	�B	�B	�'B	�-B	�9B	�?B	�?B	�FB	�LB	�RB	�RB	�^B	�dB	�jB	�wB	�}B	��B	ÖB	ĜB	ŢB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<u<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<T��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<D��<D��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<D��<#�
<D��<#�
<#�
<#�
<#�
<#�
<D��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0 dbar.                                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031447072012010314470720120103144707  AO  ARGQ                                                                        20111130141321  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130141321  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144707  IP                  G�O�G�O�G�O�                