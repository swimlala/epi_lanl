CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:42Z UW 3.1 conversion   
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
_FillValue                 �  A<   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  C$   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  TD   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  [�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  ]�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ed   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  gL   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  n�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  v�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  xl   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �8   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �H   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �L   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        �\   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        �`   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �d   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �hArgo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               �A   AO  20111130143248  20190522121827  1727_5046_182                   2C  D   APEX                            2143                            040306                          846 @� �4��1   @� ����@5�1&��c�����1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @9��@�  @�  A��A   A@  A`  A�  A�  A�  A�  A�  A���A�  A�  B   B  B  B  B   B(ffB0  B8  B?��BH  BP  BX  B`  BhffBo��Bw��B��B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^�C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C��C��C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  D   D � D ��Dy�D  D� D  D� D��D� D  D� D  D� D  D� D  D� D	  D	y�D
  D
� D  D� D  D�fDfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1y�D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9�fD:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA�fDB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Doy�Do��Dp� Dq  Dq� Dr  Dr� Ds  Dsff1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @9��@�  @�  A��A   A@  A`  A�  A�  A�  A�  A�  A���A�  A�  B   B  B  B  B   B(ffB0  B8  B?��BH  BP  BX  B`  BhffBo��Bw��B��B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^�C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C��C��C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  D   D � D ��Dy�D  D� D  D� D��D� D  D� D  D� D  D� D  D� D	  D	y�D
  D
� D  D� D  D�fDfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1y�D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9�fD:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA�fDB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Doy�Do��Dp� Dq  Dq� Dr  Dr� Ds  Dsff1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AсAсAуA�|�A�~�AхAхAч+Aч+Aщ7Aщ7AыDAыDAэPAэPAя\Aя\AыDAыDAыDAыDAыDAч+Aщ7Aщ7AёhAсAмjA�O�A��A�hsA��mA�$�A�A�A�-A�|�Aĕ�Aå�A�VA�(�A�ƨA�1'A�A�VA�ZA���A���A�bNA��A�A��\A��\A�O�A��^A��/A�ffA�%A�VA���A�O�A��RA��
A��yA��A��RA�C�A�VA���A���A�=qA���A�bA�I�A�A�v�A���A��mA���A���A���A�33A�$�A��PA���A�(�A��TA�jA�|�A���A��#A��A�hsA�%A��9A�ȴA��A�I�A�~�A��A��PA���A��TA�33A���A�%A�~�A{ƨAzM�Ay/Ax(�AwS�Avv�At��Arv�Aq�
Ap(�AlffAjM�AihsAh��Af��Ae�7Ad�RAaK�A^E�A]��A]|�A\ĜAZz�AX��AX9XAW�AW��AV�9AU��AT��AS�7ARȴAQ�mAQdZAP�`AP�+APjAO��AM�ALv�AL{AJ�jAI��AI&�AH��AG/AE��AB�9A?t�A=l�A;�;A:ffA7��A5�
A4r�A2�jA1�
A/��A.�9A-�7A,JA)��A%l�A!��A?}A��A�jA&�A
=AȴA�An�AJA�hAl�AO�A�A��A�PAĜAƨAO�A��Ar�AJA&�A$�A?}AVA��AoA
I�A	A	?}A�uA(�A1A��A��A\)An�A^5AI�AA��AĜA �AC�A �@�=q@�O�@�1'@�+@���@��h@�h@�9X@�+@@�7L@�K�@�p�@�j@�
=@�ff@�-@��@�O�@�9@���@�o@�~�@�J@���@��@�G�@�bN@�dZ@�5?@��@ݡ�@�x�@��`@܋D@��@�ƨ@�
=@���@��@�bN@�\)@�@Ԭ@��
@�n�@��@�@Ѻ^@�X@�A�@�l�@�C�@�o@�@��@θR@Ο�@�-@͡�@�x�@�1@�l�@��@ȼj@�Q�@��;@�l�@���@�~�@��@�hs@�G�@�&�@Ĭ@��@�"�@�ff@�@�bN@���@�K�@��u@��@�&�@�33@���@��P@�j@�@��@�ȴ@���@�-@�G�@�hs@�V@�V@�&�@�1'@���@��9@��m@���@�x�@�?}@�O�@��h@��^@�@���@���@�J@�^5@�@�1@�  @��@�"�@��@��@�9X@���@�G�@�b@�?}@���@���@�Q�@�
=@��y@��!@��y@�C�@�ƨ@��P@�+@��@���@�ff@��@��@��@�V@�X@���@�A�@��w@��R@�E�@��h@��@�Z@��h@���@�hs@�
=@�G�@�z�@�r�@�bN@�A�@�1'@�(�@��;@�1'@��m@���@�ƨ@�33@���@�ȴ@�@�
=@��y@�E�@��#@�{@�^5@��+@��@�M�@�X@�&�@�`B@��h@���@��@��@��#@�E�@�n�@��@�Ĝ@�o@��!@�v�@�M�@�$�@�{@���@�G�@�G�@�hs@���@��@���@�v�@�x�@�Q�@�Ĝ@�O�@��@���@���@��R@���@��+@���@�
=@��@�"�@�+@�o@�^5@�@��@�7L@�?}@�%@��@�?}@��@�1'@�33@�ff@���@���@�p�@���@�Z@���@��P@�l�@�\)@�\)@��@�"�@�S�@�l�@�l�@�t�@�t�@�\)@�C�@��@�o@�
=@���@���@��y@���@���@��+@�ff@��@�@��h@�G�@��@�Ĝ@��D1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 AсAсAуA�|�A�~�AхAхAч+Aч+Aщ7Aщ7AыDAыDAэPAэPAя\Aя\AыDAыDAыDAыDAыDAч+Aщ7Aщ7AёhAсAмjA�O�A��A�hsA��mA�$�A�A�A�-A�|�Aĕ�Aå�A�VA�(�A�ƨA�1'A�A�VA�ZA���A���A�bNA��A�A��\A��\A�O�A��^A��/A�ffA�%A�VA���A�O�A��RA��
A��yA��A��RA�C�A�VA���A���A�=qA���A�bA�I�A�A�v�A���A��mA���A���A���A�33A�$�A��PA���A�(�A��TA�jA�|�A���A��#A��A�hsA�%A��9A�ȴA��A�I�A�~�A��A��PA���A��TA�33A���A�%A�~�A{ƨAzM�Ay/Ax(�AwS�Avv�At��Arv�Aq�
Ap(�AlffAjM�AihsAh��Af��Ae�7Ad�RAaK�A^E�A]��A]|�A\ĜAZz�AX��AX9XAW�AW��AV�9AU��AT��AS�7ARȴAQ�mAQdZAP�`AP�+APjAO��AM�ALv�AL{AJ�jAI��AI&�AH��AG/AE��AB�9A?t�A=l�A;�;A:ffA7��A5�
A4r�A2�jA1�
A/��A.�9A-�7A,JA)��A%l�A!��A?}A��A�jA&�A
=AȴA�An�AJA�hAl�AO�A�A��A�PAĜAƨAO�A��Ar�AJA&�A$�A?}AVA��AoA
I�A	A	?}A�uA(�A1A��A��A\)An�A^5AI�AA��AĜA �AC�A �@�=q@�O�@�1'@�+@���@��h@�h@�9X@�+@@�7L@�K�@�p�@�j@�
=@�ff@�-@��@�O�@�9@���@�o@�~�@�J@���@��@�G�@�bN@�dZ@�5?@��@ݡ�@�x�@��`@܋D@��@�ƨ@�
=@���@��@�bN@�\)@�@Ԭ@��
@�n�@��@�@Ѻ^@�X@�A�@�l�@�C�@�o@�@��@θR@Ο�@�-@͡�@�x�@�1@�l�@��@ȼj@�Q�@��;@�l�@���@�~�@��@�hs@�G�@�&�@Ĭ@��@�"�@�ff@�@�bN@���@�K�@��u@��@�&�@�33@���@��P@�j@�@��@�ȴ@���@�-@�G�@�hs@�V@�V@�&�@�1'@���@��9@��m@���@�x�@�?}@�O�@��h@��^@�@���@���@�J@�^5@�@�1@�  @��@�"�@��@��@�9X@���@�G�@�b@�?}@���@���@�Q�@�
=@��y@��!@��y@�C�@�ƨ@��P@�+@��@���@�ff@��@��@��@�V@�X@���@�A�@��w@��R@�E�@��h@��@�Z@��h@���@�hs@�
=@�G�@�z�@�r�@�bN@�A�@�1'@�(�@��;@�1'@��m@���@�ƨ@�33@���@�ȴ@�@�
=@��y@�E�@��#@�{@�^5@��+@��@�M�@�X@�&�@�`B@��h@���@��@��@��#@�E�@�n�@��@�Ĝ@�o@��!@�v�@�M�@�$�@�{@���@�G�@�G�@�hs@���@��@���@�v�@�x�@�Q�@�Ĝ@�O�@��@���@���@��R@���@��+@���@�
=@��@�"�@�+@�o@�^5@�@��@�7L@�?}@�%@��@�?}@��@�1'@�33@�ff@���@���@�p�@���@�Z@���@��P@�l�@�\)@�\)@��@�"�@�S�@�l�@�l�@�t�@�t�@�\)@�C�@��@�o@�
=@���@���@��y@���@���@��+@�ff@��@�@��h@�G�@��@�Ĝ@��D1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��BB�BN�B�{B�FB��B��B�XB�3B��B�B{�B�B�B�bB��B��B�B�B�B��B��B��B�=B�{B��B��B��B��B��B��B��B��B��B��B�hB�Bw�B|�B�B|�By�B[#B<jB)�B�B��B�B�B�)B�HB�sB��B��B�B��BB�RB�B��B�oB�+B{�Bq�BgmB[#BH�B6FB)�B�BbBB
��B
�TB
��B
�XB
��B
{�B
^5B
8RB
�B
PB
B	��B	��B	��B	�B	�;B	�B	ɺB	�3B	��B	��B	��B	�bB	�=B	�+B	x�B	q�B	o�B	l�B	ffB	[#B	W
B	S�B	S�B	P�B	I�B	C�B	=qB	8RB	49B	0!B	-B	+B	(�B	&�B	#�B	�B	hB	\B	1B	B��B��B��B�B�TB�B��BŢB�}B�XB�FB�3B�3B�!B�B�B��B��B��B��B�uB�uB�oB�bB�PB�VB�VB�PB�PB�JB�JB�JB�=B�1B�B�B� B~�B}�B}�B{�Bz�Bx�Bw�Bv�Bu�Bt�Bs�Br�Bp�Bo�Bn�Bn�Bm�Bl�Bk�Bm�Br�Bt�Bu�Bw�Bx�Bu�Br�Bp�Bm�Bl�Bl�Bk�BiyBm�Bn�BjBl�Bo�Bq�Bn�Bm�Bm�Bq�Bt�Bv�Bz�B|�B�B�B�B�%B�%B�+B�+B�7B�=B�DB�JB�\B�bB�hB�hB�uB��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�'B�FB�XB�^B�^B�dB�dB�jB�jB�qB��B��BŢBŢBɺB��B��B��B��B�B�B�/B�;B�BB�NB�sB�B�B��B��B��B��B��B�B��B�#B�yB��B	B��B�B�;B�/B�
B��B��B�
B�B�;B�NB�mB�B�B�B�B�B��B��B��B	  B	B		7B	�B	�B	!�B	%�B	+B	,B	-B	)�B	!�B	"�B	,B	49B	7LB	5?B	C�B	N�B	K�B	H�B	F�B	F�B	F�B	M�B	T�B	ZB	[#B	]/B	_;B	aHB	bNB	`BB	]/B	`BB	k�B	o�B	p�B	p�B	r�B	p�B	n�B	l�B	l�B	m�B	u�B	� B	y�B	q�B	k�B	m�B	o�B	o�B	p�B	s�B	v�B	w�B	{�B	~�B	�B	�B	�B	�B	�7B	�DB	�JB	�JB	�PB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�-B	�FB	�RB	�FB	�'B	�B	�B	�B	�B	�B	�B	�'B	�3B	�LB	�^B	�qB	��B	ƨB	ǮB	ĜB	ĜB	ɺB	��B	��B	ǮB	ƨB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�#B	�#B	�B	�
B	�B	�B	�#B	�B	�B	�B	�B	�#B	�)B	�/B	�;B	�HB	�TB	�`B	�mB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��BB"�BS�B��B�dB��B��B�^B�FB��B�%B|�B�B�+B�uB��B�B�-B�B�B��B��B��B�JB�{B��B��B��B��B��B��B��B��B��B��B��B�1By�B�B�bB�B�BbNB>wB,B!�BB�B�B�NB�`B�sBB��B�5B��BǮB��B�3B�B��B�JB�Bv�Bl�BdZBP�B<jB.B$�B�B1BB
�yB
�5B
ÖB
�B
�+B
n�B
H�B
�B
hB
1B
B	��B	��B	�B	�HB	�)B	��B	�RB	��B	��B	��B	�uB	�PB	�bB	�B	r�B	q�B	o�B	m�B	`BB	YB	T�B	T�B	S�B	L�B	F�B	@�B	:^B	7LB	2-B	/B	,B	)�B	(�B	,B	�B	uB	{B	JB	B	B	  B��B��B�B�/B��BɺBŢB�wB�XB�LB�?B�9B�'B�B�B�B�B��B��B�{B�uB��B�uB�\B�\B�VB�VB�VB�PB�PB�PB�VB�VB�%B�B�B� B~�B}�B}�B|�Bz�By�Bw�Bw�Bv�Bt�Br�Br�Bp�Bo�Bo�Bp�Bp�Bp�Br�Bu�Bv�By�B{�Bx�Bv�Bt�Bp�Bm�Bn�Bl�Bl�Bq�Bs�Bl�Bm�Bp�Bs�Bq�Bo�Bn�Bs�Bu�Bv�Bz�B}�B�B�+B�%B�+B�+B�+B�1B�=B�JB�PB�VB�bB�hB�oB�oB�{B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�3B�RB�^B�dB�^B�dB�dB�jB�qB�wB��BĜBǮBȴB��B��B��B��B�
B�B�#B�5B�;B�HB�TB�B�B�B��B��B��B��B��B��B��B�B�ZB��B		7B��B�B�NB�BB�#B��B��B�B�B�;B�TB�mB�B�B�B�B�B��B��B��B	  B	B	%B	�B	�B	 �B	$�B	+B	-B	.B	-B	$�B	 �B	+B	49B	9XB	33B	@�B	P�B	N�B	J�B	F�B	F�B	F�B	L�B	S�B	[#B	\)B	^5B	`BB	bNB	bNB	cTB	]/B	^5B	k�B	p�B	q�B	q�B	t�B	q�B	o�B	n�B	l�B	k�B	s�B	�B	y�B	t�B	l�B	m�B	o�B	o�B	p�B	s�B	w�B	w�B	|�B	� B	�B	�B	�B	�B	�7B	�DB	�JB	�PB	�VB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�-B	�FB	�XB	�RB	�3B	�B	�B	�B	�B	�B	�B	�-B	�3B	�LB	�^B	�jB	��B	ƨB	ɺB	ƨB	ÖB	ȴB	��B	��B	ȴB	ƨB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�#B	�/B	�)B	�B	��B	�B	�/B	�#B	�#B	�B	�B	�#B	�)B	�/B	�;B	�HB	�TB	�`B	�mB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<49X<#�
<e`B<49X<�o<�o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<49X<49X<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0 dbar.                                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031447382012010314473820120103144738  AO  ARGQ                                                                        20111130143248  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130143248  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144738  IP                  G�O�G�O�G�O�                