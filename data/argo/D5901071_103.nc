CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:19Z UW 3.1 conversion   
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
_FillValue                    �DArgo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               gA   AO  20111130141512  20190522121826  1727_5046_103                   2C  D   APEX                            2143                            040306                          846 @Ԛ���`1   @Ԛ�s��@6{dZ��c�S���1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @���@���@���A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B ffB(  B0  B8  B@  BH  BPffBXffB`ffBh  Bp  Bx  B��B���B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C��3C�  C��C��C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D
��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD�fD  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$�fD%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dpy�Dq  Dq� Dr  Dr� Dr��Ds� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @���@���@���A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B ffB(  B0  B8  B@  BH  BPffBXffB`ffBh  Bp  Bx  B��B���B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C��3C�  C��C��C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D
��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD�fD  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$�fD%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dpy�Dq  Dq� Dr  Dr� Dr��Ds� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AϓuAϓuAϓuAϡ�Aϧ�Aϣ�Aϟ�Aϝ�Aϝ�Aϣ�Aϡ�AϑhAϋDAϋDAϓuAϙ�AϏ\A�|�A�ffA�bNA�dZA�dZA�ZA�A�A�A�A�?}A�7LA�33A�1'A�&�A�{A�M�A� �A���A�1'A�^5A��A�A���A��A�C�A��A�A�A��wA��;A�A�A�l�A��A��+A��TA�bNA�A�A�-A�9XA��#A�/A��A���A���A�A�A��A���A���A�+A��
A�O�A���A�?}A�A�A�n�A���A�`BA��A��\A��A���A��!A��#A�A�ffA���A�^5A�-A���A�&�A�jA��RA�"�A�K�A��A��A��A�n�A��TA�/A��A�JA�=qA��\A���A�M�A�1A�K�A�+A�JA��mA��#A���A�7LA�Q�A"�AzM�Ax1Av��Au�PAs&�Ap �Am�TAk%Ah��Af�9Ae"�Ac��Ab�/AaO�A`�/A`��A`�A_��A_A]��A\ĜA\Q�A[�7A[�AY�
AW�AU��AU/AT1AR��AMt�AK`BAJ�AI&�AG/AFjAD~�AD5?A@n�A@{A?"�A=XA< �A;hsA:��A:  A9&�A8ĜA8ffA7��A6A�A4(�A2v�A1?}A0��A0�A/�A//A.�9A-;dA,�/A,ĜA,��A,�\A,ZA,1A+�A*�jA)�A)�wA)��A)C�A'A%�TA%O�A$-A"$�A!`BAA=qAC�A�A��A~�AXA�A1A�AK�A�RA�AK�A��A�+A �AƨAp�A�HAE�AƨA��A?}AA`BA?}AVA��A�uA�A
�A
9XA	?}AhsAI�AƨAx�A��A5?Ao@�+@�z�@���@���@���@��;@�-@�"�@��/@�@�33@�h@�K�@���@�+@�R@���@�I�@��y@���@ڏ\@���@�o@֗�@�M�@�J@�hs@�j@Ӆ@��@ҧ�@��@��@�Ĝ@Л�@�b@�@���@ˍP@ə�@Ȭ@���@ư!@Ł@ċD@�(�@�33@��T@�Z@���@��@���@���@��-@��/@���@��\@��@��@�r�@��@�{@��-@��@�7L@�&�@�r�@�$�@��@��@��@�(�@��j@��@��D@�+@��+@���@��@���@�(�@���@�$�@��T@�x�@��@��@�r�@�ƨ@�o@��\@�M�@��h@�x�@�hs@��7@�/@�j@�Q�@�1'@���@��R@�~�@���@�j@��@��@��@�K�@�o@�
=@�
=@�
=@��H@��R@�^5@��@��#@��h@�`B@���@��D@�  @��m@��@��;@���@�ƨ@��F@���@��P@�\)@�@�@�"�@���@��R@�ff@��@���@��-@�/@�Ĝ@�V@�?}@�&�@�%@�Ĝ@��D@�bN@�1'@��@���@���@��@�o@�^5@�@�x�@��D@���@�"�@��@��H@���@���@��#@��h@�`B@�7L@���@��@��w@���@�J@��`@�9X@��
@���@�\)@�K�@�o@��R@���@�ȴ@���@��\@��+@�=q@�5?@�$�@���@��7@�?}@���@��u@��u@���@�j@��;@�l�@��+@�@�/@���@��9@���@�r�@�Z@���@�b@�9X@�@�+@��m@�b@��m@���@�C�@�{@��@��@�M�@�V@�=q@�-@�=q@�E�@�V@�ff@�V@�$�@�@���@���@�@�x�@�7L@��@��@�%@�j@�@|�@;d@�@~��@~ff@~$�@}@}��@}/@|��@|�@|�D111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  AϓuAϓuAϓuAϡ�Aϧ�Aϣ�Aϟ�Aϝ�Aϝ�Aϣ�Aϡ�AϑhAϋDAϋDAϓuAϙ�AϏ\A�|�A�ffA�bNA�dZA�dZA�ZA�A�A�A�A�?}A�7LA�33A�1'A�&�A�{A�M�A� �A���A�1'A�^5A��A�A���A��A�C�A��A�A�A��wA��;A�A�A�l�A��A��+A��TA�bNA�A�A�-A�9XA��#A�/A��A���A���A�A�A��A���A���A�+A��
A�O�A���A�?}A�A�A�n�A���A�`BA��A��\A��A���A��!A��#A�A�ffA���A�^5A�-A���A�&�A�jA��RA�"�A�K�A��A��A��A�n�A��TA�/A��A�JA�=qA��\A���A�M�A�1A�K�A�+A�JA��mA��#A���A�7LA�Q�A"�AzM�Ax1Av��Au�PAs&�Ap �Am�TAk%Ah��Af�9Ae"�Ac��Ab�/AaO�A`�/A`��A`�A_��A_A]��A\ĜA\Q�A[�7A[�AY�
AW�AU��AU/AT1AR��AMt�AK`BAJ�AI&�AG/AFjAD~�AD5?A@n�A@{A?"�A=XA< �A;hsA:��A:  A9&�A8ĜA8ffA7��A6A�A4(�A2v�A1?}A0��A0�A/�A//A.�9A-;dA,�/A,ĜA,��A,�\A,ZA,1A+�A*�jA)�A)�wA)��A)C�A'A%�TA%O�A$-A"$�A!`BAA=qAC�A�A��A~�AXA�A1A�AK�A�RA�AK�A��A�+A �AƨAp�A�HAE�AƨA��A?}AA`BA?}AVA��A�uA�A
�A
9XA	?}AhsAI�AƨAx�A��A5?Ao@�+@�z�@���@���@���@��;@�-@�"�@��/@�@�33@�h@�K�@���@�+@�R@���@�I�@��y@���@ڏ\@���@�o@֗�@�M�@�J@�hs@�j@Ӆ@��@ҧ�@��@��@�Ĝ@Л�@�b@�@���@ˍP@ə�@Ȭ@���@ư!@Ł@ċD@�(�@�33@��T@�Z@���@��@���@���@��-@��/@���@��\@��@��@�r�@��@�{@��-@��@�7L@�&�@�r�@�$�@��@��@��@�(�@��j@��@��D@�+@��+@���@��@���@�(�@���@�$�@��T@�x�@��@��@�r�@�ƨ@�o@��\@�M�@��h@�x�@�hs@��7@�/@�j@�Q�@�1'@���@��R@�~�@���@�j@��@��@��@�K�@�o@�
=@�
=@�
=@��H@��R@�^5@��@��#@��h@�`B@���@��D@�  @��m@��@��;@���@�ƨ@��F@���@��P@�\)@�@�@�"�@���@��R@�ff@��@���@��-@�/@�Ĝ@�V@�?}@�&�@�%@�Ĝ@��D@�bN@�1'@��@���@���@��@�o@�^5@�@�x�@��D@���@�"�@��@��H@���@���@��#@��h@�`B@�7L@���@��@��w@���@�J@��`@�9X@��
@���@�\)@�K�@�o@��R@���@�ȴ@���@��\@��+@�=q@�5?@�$�@���@��7@�?}@���@��u@��u@���@�j@��;@�l�@��+@�@�/@���@��9@���@�r�@�Z@���@�b@�9X@�@�+@��m@�b@��m@���@�C�@�{@��@��@�M�@�V@�=q@�-@�=q@�E�@�V@�ff@�V@�$�@�@���@���@�@�x�@�7L@��@��@�%@�j@�@|�@;d@�@~��@~ff@~$�@}@}��@}/@|��@|�@|�D111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�\B�\B�\B�\B�\B�\B�\B�\B�VB�\B�VB�VB�VB�VB�VB�VB�VB�VB�VB�PB�PB�PB�JB�PB�PB�PB�VB�VB�VB�\B�VB�\B�FB��B��B�B�/B�#B�
B��BȴBƨBǮBĜB��BƨB�qB�dB�^B�RB�LB�FB�-B�-B�!B�B�B�B��B��B��B��B��B��B��B�{B�uB�bB�VB�+B� Br�BhsBe`BbNB\)BXBA�B9XB/B&�B�B�BhBDBB��B�yB��B��B�uB~�Bl�BaHBF�B:^B33B�BB
�sB
�B
�^B
��B
�B
r�B
bNB
_;B
^5B
\)B
S�B
>wB
%�B
hB
VB
�B
uB
1B	��B	�B	�#B	��B	B	�XB	�'B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�uB	�\B	�7B	t�B	hsB	aHB	S�B	C�B	�B	DB	B��B	
=B	
=B	B	B�B�B�NB��B��B��B��B��B��B��B�5B�HB�sB�BB�)B�B�
B�B��B��B��BȴBȴBǮBƨBƨBĜBB��B�wB�jB�dB�^B�LB�!B�B��B��B��B��B��B�hB�bB�\B�VB�DB�7B�+B�B�B~�B|�Bz�By�Bx�Bw�Bv�Bu�Bt�Br�Bq�Bn�Bk�BhsBgmBffBffBe`BdZBcTB_;B_;B]/B[#BXBVBT�BS�BO�BI�BH�BD�B;dB6FB6FB7LB5?B1'B.B,B+B'�B$�B#�B"�B"�B"�B �B �B�B�B�B�B�B�B�B�B�B�B�B�B�B �B!�B!�B!�B�B!�B!�B!�B"�B"�B!�B!�B#�B%�B%�B%�B%�B(�B'�B)�B-B-B-B/B0!B2-B7LB9XB8RB7LB9XB:^B;dB<jB<jB?}BF�BQ�B`BBcTBe`BhsBiyBn�Bv�By�B}�B�B�B�B�=B�DB�JB�PB�\B�bB�bB�bB�hB�uB��B��B��B��B��B��B��B��B�B�'B�3B�?B�wB��B��B��B�B�B�)B�/B�/B�/B�;B�yB�B�B�B��B��B��B	  B	%B	+B	+B	1B	1B		7B		7B	
=B	DB	JB	\B	�B	&�B	(�B	+B	+B	0!B	33B	33B	6FB	:^B	@�B	B�B	D�B	D�B	E�B	F�B	I�B	M�B	O�B	P�B	Q�B	Q�B	R�B	S�B	T�B	VB	XB	[#B	^5B	_;B	_;B	`BB	`BB	bNB	cTB	dZB	dZB	e`B	ffB	gmB	gmB	ffB	gmB	gmB	ffB	ffB	gmB	gmB	hsB	jB	k�B	l�B	m�B	n�B	n�B	t�B	u�B	u�B	v�B	w�B	y�B	y�B	z�B	{�B	|�B	|�B	|�B	}�B	~�B	�B	�B	�+B	�DB	�DB	�DB	�PB	�PB	�bB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�9B	�RB	�XB	�^B	�dB	�jB	�wB	��B	ÖB	ĜB	ŢB	ŢB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�)B	�5B	�;B	�TB	�ZB	�ZB	�`111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�\B�\B�\B�\B�\B�\B�\B�\B�VB�\B�VB�VB�VB�VB�VB�VB�VB�VB�VB�PB�PB�PB�JB�PB�PB�PB�VB�VB�VB�\B�\B��B�wB��B�B�BB�BB�5B�#B��B��B��B��B��B��B��B�}B�qB�jB�^B�XB�^B�^B�FB�-B�!B�B�B�3B�B��B��B��B��B��B��B��B�uB�{B�JB�=B{�Bk�BgmBdZBcTB`BBE�B=qB2-B)�B�B�BuBVBB��B�B�B�'B��B�Bo�BjBJ�B<jB9XB(�BJB
�B
�;B
ȴB
��B
�DB
w�B
cTB
_;B
^5B
^5B
\)B
E�B
1'B
�B
hB
�B
�B
bB	��B	�B	�NB	��B	ǮB	�jB	�9B	�!B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�{B	�hB	�JB	x�B	jB	cTB	W
B	N�B	"�B	PB	B	  B	JB	\B	B	\B�B�B�mB��B��B��B��B��B��B�B�BB�`B�B�`B�;B�B�
B�B��B��B��BɺBȴBǮBƨBǮBŢBŢB��B��B�qB�jB�dB�jB�FB�B�B��B��B��B��B�{B�hB�bB�\B�\B�=B�=B�7B�B�B~�B|�Bz�By�Bx�Bw�Bv�Bv�Bt�Bs�Bq�Bo�Bl�BhsBgmBgmBffBe`Be`BcTB`BB`BBaHB[#BXBVBVBXBM�BM�BH�B@�B:^B9XB9XB9XB5?B2-B.B,B+B(�B'�B%�B#�B$�B#�B#�B#�B!�B!�B!�B �B �B�B �B�B�B �B �B �B!�B"�B!�B"�B"�B"�B#�B$�B$�B$�B#�B#�B%�B&�B%�B'�B(�B+B+B,B.B/B.B1'B2-B49B8RB:^B9XB:^B:^B:^B<jB<jB=qBC�BF�BO�B`BBcTBdZBhsBjBp�Bw�B{�B~�B�B�B�%B�=B�DB�PB�VB�bB�bB�hB�oB�oB�{B��B��B��B��B��B��B��B��B�!B�-B�9B�LB��B��B��B��B�B�B�)B�/B�/B�/B�;B�B�B�B�B��B��B��B	B	%B	+B	+B	1B	1B		7B		7B	
=B	DB	PB	\B	�B	&�B	)�B	,B	,B	0!B	33B	49B	7LB	:^B	@�B	B�B	D�B	D�B	E�B	F�B	I�B	M�B	O�B	P�B	Q�B	R�B	S�B	T�B	VB	W
B	YB	\)B	^5B	_;B	_;B	`BB	aHB	cTB	cTB	dZB	e`B	ffB	ffB	gmB	jB	hsB	hsB	hsB	ffB	ffB	gmB	gmB	iyB	jB	k�B	l�B	m�B	n�B	o�B	t�B	u�B	v�B	w�B	x�B	z�B	y�B	z�B	{�B	|�B	}�B	}�B	� B	� B	�B	�B	�+B	�DB	�DB	�DB	�VB	�PB	�bB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�9B	�RB	�XB	�^B	�dB	�jB	�wB	��B	ÖB	ĜB	ŢB	ŢB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�)B	�5B	�;B	�TB	�ZB	�ZB	�`111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<49X<#�
<#�
<#�
<e`B<49X<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0 dbar.                                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031447102012010314471020120103144710  AO  ARGQ                                                                        20111130141512  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130141512  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144710  IP                  G�O�G�O�G�O�                