CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:08Z UW 3.1 conversion   
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
_FillValue                    �DArgo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               =A   AO  20111130140502  20190522121826  1727_5046_061                   2C  D   APEX                            2143                            040306                          846 @�d|�� 1   @�d}/h@@7A$�/�c�V�u1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  C   C  C  C�C  C
  C  C�fC  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:�C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ�C\  C]�fC_�fCb  Cd  Ce�fCh  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��3C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D
��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D!��D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0�fD1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk�fDl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  DyL�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  C   C  C  C�C  C
  C  C�fC  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:�C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ�C\  C]�fC_�fCb  Cd  Ce�fCh  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��3C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D
��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D!��D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0�fD1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk�fDl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  DyL�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�`BA��;A���A�^5A�?}A�33A�-A�(�A�$�A�"�A��A��A��A�bA�VA�VA�VA�VA�JA�1A��A��A�ƨA���A��+A�hsA�VA�K�A�?}A�9XA�1'A�(�A��A��A��A�{A��A�{A�VA�JA�1A�A�%A�1A�1A�%A�%A�%A�JA�oA�oA�{A��A��A��A��A�oA�bA�bA�bA�VA�
=A���A���A��A��`A��/A���A���A��FA��\A�z�A�x�A�r�A�Q�A�$�A��9A�-A���A�%A���A��A�r�A�hsA���A��\A�E�A�/A��A��`A��+A�?}A��A�^5A��
A�  A��jA��`A��DA�9XA�I�A��^A�bNA�M�A�t�A��A�A���A��A�dZA��jA�(�A�"�A��A���A��;A�jA�  A�r�A�^5A��A�=qA�/A� �A�\)A��A��/A�x�A�Q�A�{A�O�A�~�A|-Aw��Aux�AuoAt-Aq��Ao��AoG�An1Aj~�Ah��Af�DAep�Adv�Ac�-Ac+Aa�A_`BA[\)AY��AY�AX�AWdZAVffAT�yAS7LARVAR=qAR{AQ��AP��AN��AMG�AK�;AK�AKdZAKVAJM�AI�
AIK�AH�\AG�-AF�DAD�/AB��AA��A@��A@z�A?|�A=��A<��A;�PA:�`A:VA8�uA7&�A69XA5&�A3��A2�`A0��A/��A.��A-�A,ZA+
=A*�9A*n�A*A�A*1'A*5?A)�A(VA(1'A(  A'��A&��A&M�A$ĜA#��A"��A"�DA"ZA!��A!A �\A�A��A��A��A�A33AȴA��AbAhsAQ�A��A�Ap�A
=A��A^5A|�AI�A^5A�A"�A��AbAC�A��AbNA��A7LA
��A	��A	\)A��A1A~�@�\)@�x�@�j@�$�@�V@�V@�Z@�v�@�Z@�Q�@��
@�{@�%@�@�b@�+@�9X@��H@�M�@��@��@�l�@�1@�o@�5?@�?}@��`@Ь@���@ͩ�@ʧ�@Ɂ@�V@ȴ9@���@�&�@�Z@��
@�C�@�5?@�`B@���@���@���@�;d@��y@�^5@��#@��`@�9X@��@��P@���@��9@��
@���@�hs@�Q�@�o@���@�V@�x�@�%@��u@��
@�+@��+@��^@�r�@���@�@��@�Q�@���@���@�@�X@���@�z�@�A�@�  @���@��H@���@��@���@��9@��@�M�@���@���@�O�@�V@�\)@���@�+@��@��w@��
@��m@�1@�\)@��@���@��@�ƨ@���@��-@��@�\)@��\@��@�G�@�/@��@�V@���@��j@���@��u@�z�@�Z@�I�@�(�@��@��@�1@��@��
@��P@�"�@��R@�V@��T@��#@��T@�$�@���@�/@��@�j@���@���@���@�t�@��P@��@� �@��@��;@��P@��@���@�M�@�$�@���@�hs@���@�Q�@�(�@�  @��F@�S�@���@��R@���@�ff@�J@���@�hs@�p�@�hs@���@�%@��-@�@��@�@��#@��-@��@�G�@�7L@�?}@��@�%@��m@���@���@�|�@�\)@�;d@�33@�
=@�J@�?}@��/@���@�Ĝ@���@�j@��;@��F@��@���@�t�@�\)@�o@��y@���@��!@��!@�~�@�$�@��T@��^@���@��@�p�@�O�@�?}@��@�%@��/@��@�bN@�I�@�A�@�1'@�b@��;@��F@�{111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�`BA��;A���A�^5A�?}A�33A�-A�(�A�$�A�"�A��A��A��A�bA�VA�VA�VA�VA�JA�1A��A��A�ƨA���A��+A�hsA�VA�K�A�?}A�9XA�1'A�(�A��A��A��A�{A��A�{A�VA�JA�1A�A�%A�1A�1A�%A�%A�%A�JA�oA�oA�{A��A��A��A��A�oA�bA�bA�bA�VA�
=A���A���A��A��`A��/A���A���A��FA��\A�z�A�x�A�r�A�Q�A�$�A��9A�-A���A�%A���A��A�r�A�hsA���A��\A�E�A�/A��A��`A��+A�?}A��A�^5A��
A�  A��jA��`A��DA�9XA�I�A��^A�bNA�M�A�t�A��A�A���A��A�dZA��jA�(�A�"�A��A���A��;A�jA�  A�r�A�^5A��A�=qA�/A� �A�\)A��A��/A�x�A�Q�A�{A�O�A�~�A|-Aw��Aux�AuoAt-Aq��Ao��AoG�An1Aj~�Ah��Af�DAep�Adv�Ac�-Ac+Aa�A_`BA[\)AY��AY�AX�AWdZAVffAT�yAS7LARVAR=qAR{AQ��AP��AN��AMG�AK�;AK�AKdZAKVAJM�AI�
AIK�AH�\AG�-AF�DAD�/AB��AA��A@��A@z�A?|�A=��A<��A;�PA:�`A:VA8�uA7&�A69XA5&�A3��A2�`A0��A/��A.��A-�A,ZA+
=A*�9A*n�A*A�A*1'A*5?A)�A(VA(1'A(  A'��A&��A&M�A$ĜA#��A"��A"�DA"ZA!��A!A �\A�A��A��A��A�A33AȴA��AbAhsAQ�A��A�Ap�A
=A��A^5A|�AI�A^5A�A"�A��AbAC�A��AbNA��A7LA
��A	��A	\)A��A1A~�@�\)@�x�@�j@�$�@�V@�V@�Z@�v�@�Z@�Q�@��
@�{@�%@�@�b@�+@�9X@��H@�M�@��@��@�l�@�1@�o@�5?@�?}@��`@Ь@���@ͩ�@ʧ�@Ɂ@�V@ȴ9@���@�&�@�Z@��
@�C�@�5?@�`B@���@���@���@�;d@��y@�^5@��#@��`@�9X@��@��P@���@��9@��
@���@�hs@�Q�@�o@���@�V@�x�@�%@��u@��
@�+@��+@��^@�r�@���@�@��@�Q�@���@���@�@�X@���@�z�@�A�@�  @���@��H@���@��@���@��9@��@�M�@���@���@�O�@�V@�\)@���@�+@��@��w@��
@��m@�1@�\)@��@���@��@�ƨ@���@��-@��@�\)@��\@��@�G�@�/@��@�V@���@��j@���@��u@�z�@�Z@�I�@�(�@��@��@�1@��@��
@��P@�"�@��R@�V@��T@��#@��T@�$�@���@�/@��@�j@���@���@���@�t�@��P@��@� �@��@��;@��P@��@���@�M�@�$�@���@�hs@���@�Q�@�(�@�  @��F@�S�@���@��R@���@�ff@�J@���@�hs@�p�@�hs@���@�%@��-@�@��@�@��#@��-@��@�G�@�7L@�?}@��@�%@��m@���@���@�|�@�\)@�;d@�33@�
=@�J@�?}@��/@���@�Ĝ@���@�j@��;@��F@��@���@�t�@�\)@�o@��y@���@��!@��!@�~�@�$�@��T@��^@���@��@�p�@�O�@�?}@��@�%@��/@��@�bN@�I�@�A�@�1'@�b@��;@��F@�{111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBu�Bt�Bt�Bw�Bx�Bw�Bx�Bx�Bx�By�By�Bz�B{�B{�B|�B|�B|�B}�B~�B� B� B�B�B�B�B�B�B� B� B� B� B~�B~�B~�B~�B� B� B� B� B� B� B� B�B�B�B�B�B�B�B�B�B�+B�1B�=B�DB�DB�DB�DB�DB�DB�DB�JB�JB�\B�bB�oB�{B��B��B��B��B��B��B��B��B�B�RB�}BÖBȴBɺBȴBŢB�}B�FB�B��B��B��B��B�B��B��B��B��B�\B�B|�Bw�Br�BffBT�BN�BK�B@�B)�B�BDB��B��B�sB��B�B�{BgmBT�BL�BD�B?}B.B�BbB
�B
�
B
ǮB
�FB
��B
�bB
�JB
�B
s�B
[#B
;dB
#�B
�B
oB
JB
B	��B	�B	�mB	�BB	ȴB	�qB	�?B	�-B	�wB	ɺB	ɺB	�'B	�DB	|�B	}�B	�B	z�B	{�B	�B	|�B	{�B	|�B	|�B	{�B	v�B	n�B	jB	bNB	`BB	^5B	\)B	W
B	R�B	N�B	H�B	C�B	<jB	49B	-B	'�B	&�B	#�B	�B	�B	oB	\B	PB		7B	B��B��B��B�B�B�BB�)B�
B��B��B��B��B��B��B��BɺBƨBĜBÖB��B�wB�dB�RB�-B�B�B��B��B��B��B��B��B��B��B�hB�VB�JB�DB�=B�1B�%B�B�B�B�B� B}�Bz�Bt�Bq�Bn�Bm�Bl�Bk�BiyBhsBhsBgmBhsBhsBgmBffBdZB\)BS�BL�BG�BF�BD�BB�BL�BXBVBP�BH�B:^B33B1'B33B49B7LB9XB7LB9XB8RB6FB0!B)�B(�B,B,B.B.B,B-B.B-B.B/B.B2-B5?B7LB8RB7LB9XB9XB;dB;dB?}BG�BJ�BJ�BK�BJ�BH�BJ�BN�BO�BS�BVBW
BYB\)B^5B^5B^5B`BBaHBbNBcTBdZBdZBdZBffBk�Bn�Br�Bu�Bu�Bz�B~�B�B�+B�1B�JB�hB�oB�{B��B��B��B��B��B�-B�^B�RBB��B�
B�5B�BB�sB�B�B�B�B�B�B�B�B�sB�`B�HB�5B�#B�NB�mB�B��B��B��B	B	B	%B	+B	1B		7B		7B	
=B	DB	JB	VB	oB	�B	�B	!�B	#�B	$�B	%�B	&�B	&�B	+B	.B	,B	1'B	1'B	33B	49B	49B	49B	6FB	9XB	<jB	A�B	B�B	B�B	C�B	C�B	B�B	B�B	C�B	H�B	O�B	P�B	Q�B	R�B	W
B	]/B	_;B	`BB	bNB	bNB	ffB	gmB	gmB	hsB	k�B	m�B	q�B	w�B	z�B	~�B	� B	�B	�B	�B	�%B	�+B	�+B	�1B	�7B	�DB	�DB	�DB	�DB	�DB	�JB	�JB	�DB	�PB	�\B	�oB	�hB	�oB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�'B	�-B	�3B	�9B	�?B	�?B	�LB	�XB	�dB	�jB	�jB	�qB	�qB	�qB	�wB	��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  Bw�Bu�Bu�Bx�Bx�Bw�Bx�Bx�Bx�By�By�Bz�B{�B{�B|�B|�B|�B}�B~�B�B� B�B�B�B�B�B�B� B� B� B� B~�B~�B~�B~�B� B� B� B� B� B� B� B�B�B�B�B�B�B�B�B�B�+B�1B�=B�DB�DB�DB�DB�DB�DB�DB�JB�JB�\B�bB�oB�{B��B��B��B��B��B��B��B�B�B�^B��BŢBɺB��BɺBɺBǮB��B�?B�!B�'B�FB�!B�B�B�B��B��B�{B�7B~�Bx�Bv�Bm�BW
BO�BP�BG�B0!B!�BhB��B��B�B�#B�jB��Bq�BXBO�BH�BE�B6FB�B�B
��B
�B
��B
��B
��B
�hB
�PB
�7B
~�B
iyB
G�B
,B
�B
�B
{B
%B	��B	��B	�B	�mB	��B	��B	�RB	�?B	��B	��B	��B	�qB	�\B	}�B	~�B	�B	|�B	� B	�+B	~�B	{�B	}�B	}�B	}�B	{�B	s�B	n�B	cTB	`BB	_;B	^5B	XB	T�B	P�B	K�B	G�B	A�B	:^B	0!B	+B	(�B	&�B	#�B	�B	�B	hB	\B	VB	+B	B��B��B�B�B�ZB�;B�#B�
B��B��B��B��B��B��B��B��BŢBĜBÖB��B�qB�qB�FB�'B�B�B�B��B��B��B��B��B��B��B�uB�VB�JB�JB�DB�=B�%B�B�B�B�B� B~�B�Bx�Bq�Bo�Bn�Bm�Bl�BjBiyBiyBjBiyBiyBhsBk�BcTBXBS�BJ�BH�BG�BD�BL�BYBYBT�BN�BA�B6FB33B49B5?B9XB>wB9XB:^B9XB8RB5?B.B0!B,B-B/B/B-B0!B2-B/B/B0!B1'B5?B6FB8RB9XB9XB:^B:^B<jB=qB@�BH�BK�BK�BM�BK�BH�BK�BP�BR�BVBXBYB[#B^5B_;B_;B`BBaHBbNBcTBdZBe`BffBffBiyBk�Bp�Bt�Bv�Bw�B{�B� B�B�1B�1B�JB�oB�uB��B��B��B��B��B��B�'B�jB�XB��B��B�
B�;B�;B�sB�B�B�B�B�B�B�B�B�B�mB�TB�BB�)B�ZB�mB�B��B��B��B	B	B	%B	+B	1B		7B		7B	
=B	DB	JB	VB	oB	�B	 �B	"�B	$�B	%�B	%�B	&�B	&�B	,B	/B	,B	2-B	2-B	33B	49B	49B	49B	6FB	9XB	<jB	A�B	C�B	C�B	D�B	D�B	B�B	C�B	C�B	I�B	P�B	P�B	Q�B	S�B	XB	^5B	`BB	`BB	bNB	cTB	gmB	gmB	gmB	hsB	l�B	m�B	p�B	w�B	z�B	~�B	� B	�B	�B	�B	�%B	�+B	�+B	�1B	�DB	�JB	�DB	�DB	�DB	�DB	�JB	�JB	�PB	�VB	�bB	�oB	�hB	�oB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�'B	�-B	�3B	�9B	�?B	�?B	�RB	�^B	�dB	�jB	�jB	�qB	�qB	�qB	�wB	��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<e`B<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<D��<T��<u<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<49X<e`B<D��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<49X<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0 dbar.                                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031446552012010314465520120103144655  AO  ARGQ                                                                        20111130140502  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130140502  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144655  IP                  G�O�G�O�G�O�                