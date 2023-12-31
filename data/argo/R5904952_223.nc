CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:05:56Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7(   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7h   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8,   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           80   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    88   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8<   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8D   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8L   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8T   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8X   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8`   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9`   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9d   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9h   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9l   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  A4   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C(   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  T�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \h   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ^\   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  f    TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  o�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  w�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  y�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �T   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �H   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �<   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �<   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �<   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �<   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �h   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �l   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �p   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �t   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �x   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181005190556  20181005190556  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @��)�^�L1   @��*O�@@0׍O�;d�c�r� Ĝ1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @�ff@�  A   A   A@  A`  A~ffA�  A�  A�  A�  A�  A�  A�  B   B  B  B  B ffB(  B/��B7��B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B���B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�33B�33B�33B�33B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C�fC  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Ci�fCl  Cn  Cp�Cr�Ct�Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C��C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C��3C��3D � D  D� D  D� D  D�fD  Dy�D��D� D  D�fDfDy�D  D� D	fD	�fD
fD
�fD  D� D  D� D��Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D��Dy�D  D� DfD� D��D� D  D� D  D�fDfD�fD  Dy�D   D � D!  D!� D"  D"y�D"��D#� D$  D$y�D$��D%� D&  D&�fD'  D'� D(fD(� D)  D)� D*  D*� D+  D+y�D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1y�D2  D2�fD3  D3y�D3��D4y�D5  D5� D5��D6� D7  D7y�D8  D8�fD9fD9�fD:  D:� D:��D;� D<fD<�fD=fD=�fD>fD>� D?  D?� D@  D@� DA  DA� DB  DB�fDCfDC� DD  DD� DE  DE� DF  DF� DG  DG�fDHfDH�fDIfDI� DI��DJy�DJ��DKy�DL  DL� DMfDM� DN  DN� DO  DO�fDP  DPy�DQ  DQ� DR  DR� DS  DSy�DT  DT�fDUfDU� DV  DV� DW  DW�fDX  DX� DY  DYy�DZ  DZ� D[  D[� D\  D\y�D]  D]� D]��D^� D_  D_y�D_��D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Df��Dgy�Dg��Dhy�Dh��Diy�Dj  Dj� Dk  Dk� Dl  Dly�Dm  Dm� Dn  Dn� Dn��Doy�Dp  Dp�fDq  Dq� Dr  Dr� Ds  Ds� DtfDt� Du  Du� Dv  Dv� Dw  Dwy�Dw�3Dy�D�J=11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�Q�@��A��A$��AD��Ad��A��A�z�A�z�A�z�A�z�A�z�A�z�A�z�B=qB	=qB=qB=qB!��B)=qB0�B8�BA=qBI=qBQ=qBY=qBa=qBi=qBq=qBy=qB���B���B�k�B���B���B���B���B���B���B���B���B���B���B���B���B���B�k�BĞ�BȞ�B̞�BО�BԞ�B؞�B���B���B���B���B잸B�B���B���B���C O\CO\CO\CO\CO\C
O\CO\CO\CO\CO\CO\CO\CO\CO\C5�CO\C O\C"O\C$O\C&O\C(O\C*O\C,O\C.O\C0O\C2O\C4O\C6O\C8O\C:O\C<O\C>O\C@O\CBO\CDO\CFO\CHO\CJO\CLO\CNO\CPO\CRO\CTO\CVO\CXO\CZO\C\O\C^O\C`O\CbO\CdO\CfO\ChO\Cj5�ClO\CnO\Cph�Crh�Cth�CvO\CxO\CzO\C|O\C~O\C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C��C��C��C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C��C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C��C��C�'�C�'�C�'�C�'�C�'�C�'�C�4{C�4{C�'�C�'�C�'�C�'�C�4{C�'�C�4{C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�4{C�'�C��C��C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�4{C�'�C�'�C�'�C�'�C�'�C�'�C�'�C��C�'�C�'�C�'�C�'�C��C��C�'�C�'�C�'�C�'�C�'�C�4{C�4{C�'�C�'�C�'�C�'�C�'�C�'�C�'�C��C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�4{C�'�C�'�C�'�C�'�C�'�C��C�'�C�'�C�'�C�'�C�'�C�'�C��D qD ��D�D��D�D��D�D�=D�D�qDqD��D�D�=D=D�qD�D��D	=D	�=D
=D
�=D�D��D�D��DqD�qD�D��D�D��D�D��D�D��D�D��D�D��D�D�qD�D��D�D��D�D��DqD�qD�D��D=D��DqD��D�D��D�D�=D=D�=D�D�qD �D ��D!�D!��D"�D"�qD#qD#��D$�D$�qD%qD%��D&�D&�=D'�D'��D(=D(��D)�D)��D*�D*��D+�D+�qD,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1�qD2�D2�=D3�D3�qD4qD4�qD5�D5��D6qD6��D7�D7�qD8�D8�=D9=D9�=D:�D:��D;qD;��D<=D<�=D==D=�=D>=D>��D?�D?��D@�D@��DA�DA��DB�DB�=DC=DC��DD�DD��DE�DE��DF�DF��DG�DG�=DH=DH�=DI=DI��DJqDJ�qDKqDK�qDL�DL��DM=DM��DN�DN��DO�DO�=DP�DP�qDQ�DQ��DR�DR��DS�DS�qDT�DT�=DU=DU��DV�DV��DW�DW�=DX�DX��DY�DY�qDZ�DZ��D[�D[��D\�D\�qD]�D]��D^qD^��D_�D_�qD`qD`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��DgqDg�qDhqDh�qDiqDi�qDj�Dj��Dk�Dk��Dl�Dl�qDm�Dm��Dn�Dn��DoqDo�qDp�Dp�=Dq�Dq��Dr�Dr��Ds�Ds��Dt=Dt��Du�Du��Dv�Dv��Dw�Dw�qDw�
Dy��D�T)11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AͰ!A̼jA�ƨA��yA�XA�33A�AɅAȏ\A�M�A��/A�^5A�JA���A�ĜAƲ-AƮAƥ�AƑhAƇ+A�~�A�r�A�`BA�33A�{A�hsAƃA�^5A���A�33AƾwA�S�A�  A�33AŬAŅA��
A��A��A�~�A�`BA���Aò-A�{A�A�7LA�/A�(�A�33A��mA��`A��-A��A�ZA��mA��A�XA�Q�A��A���A�l�A��A�\)A�ĜA���A��^A���A��-A��A���A��!A�|�A��FA���A���A�JA���A�?}A��uA��`A�I�A���A�|�A�
=A�G�A�bNA��mA���A���A��jA��
A�5?A�33A���A��DA�O�A��A�33A��A��hA�~�A��-A�9XAC�A}�-Azn�Ax��Au;dAp��AmS�Al�DAj��Ag��Ae��Aa�A^{A[�
AX�AS��ARE�AP�`AP-AN{AL�9AK��AG�^AE�ABbA?�mA>VA<=qA;oA:^5A9��A9/A8z�A7p�A7VA6v�A5XA4$�A3��A3;dA2�9A/�mA/C�A,z�A,  A* �A(�yA&��A%K�A"r�A!��A!
=A|�A��AE�A�DA�^A�9A�
A�PA&�A�A��A�;A�yAz�A��Al�A��AĜAt�AjAbA�AbNAJA��A1AȴA
��A
�AoA1'A�jA�
AZA|�A%@���@�n�@��@���@�J@�O�@��9@��
@���@�^5@�|�@�p�@��@�h@��@��@��@�9@�@�Ĝ@�(�@柾@�-@�&�@�Q�@��
@�dZ@�K�@�33@��@�=q@ᙚ@��D@�1@�t�@���@�Z@۝�@��@�J@�X@��@���@�j@��
@֏\@ղ-@�hs@ԛ�@�l�@ҧ�@�O�@мj@Ͼw@��@�n�@��T@Ͳ-@�p�@�&�@�I�@�K�@ʟ�@�ff@�^5@�V@�M�@�5?@�@��#@ɉ7@���@ț�@�(�@��;@ǶF@�S�@�@���@Ɨ�@�M�@�O�@��`@��`@�bN@�  @Ý�@�S�@�"�@��@�&�@�&�@��h@���@��D@�1'@���@��@��@�G�@��@�1'@��@�@���@��m@�V@��#@��h@�bN@�(�@�j@�Z@���@�"�@�hs@�dZ@�"�@�C�@�l�@�t�@���@��F@��
@��m@��w@�|�@�ȴ@�7L@�Q�@��m@�\)@�K�@�+@��@��@���@�V@�$�@�{@�@��@��#@�@���@�O�@���@�I�@�ƨ@��F@�K�@�@�$�@�7L@�V@�/@���@��
@��9@��@�1'@�b@���@���@��@�~�@��^@���@�I�@� �@�b@���@��m@���@�;d@��R@��H@���@��y@��y@���@��@�~�@�@�5?@��@���@���@�{@�J@���@�hs@��@�r�@��@���@�
=@�~�@�=q@�@��@���@�/@��9@��D@�z�@�j@�(�@��@�ƨ@��@�33@��R@�n�@�M�@�@���@���@��h@�X@�%@��@�dZ@��@�C�@�@��@�M�@��^@��@�G�@�?}@��@��@���@��j@��D@�1@�ƨ@��@�S�@��y@��@���@��@�ȴ@���@��\@�^5@�M�@�-@�`B@���@�Z@�(�@���@�ƨ@���@�33@��@��!@�5?@��7@�/@���@�I�@�1'@�  @���@��@�S�@�C�@�
=@���@���@�v�@�-@�@��-@���@���@���@���@���@�p�@�O�@�%@���@��D@�bN@� �@���@��;@��F@��P@�l�@�dZ@���@��!@���@��\@�$�@���@��h@�p�@}�=11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   AͰ!A̼jA�ƨA��yA�XA�33A�AɅAȏ\A�M�A��/A�^5A�JA���A�ĜAƲ-AƮAƥ�AƑhAƇ+A�~�A�r�A�`BA�33A�{A�hsAƃA�^5A���A�33AƾwA�S�A�  A�33AŬAŅA��
A��A��A�~�A�`BA���Aò-A�{A�A�7LA�/A�(�A�33A��mA��`A��-A��A�ZA��mA��A�XA�Q�A��A���A�l�A��A�\)A�ĜA���A��^A���A��-A��A���A��!A�|�A��FA���A���A�JA���A�?}A��uA��`A�I�A���A�|�A�
=A�G�A�bNA��mA���A���A��jA��
A�5?A�33A���A��DA�O�A��A�33A��A��hA�~�A��-A�9XAC�A}�-Azn�Ax��Au;dAp��AmS�Al�DAj��Ag��Ae��Aa�A^{A[�
AX�AS��ARE�AP�`AP-AN{AL�9AK��AG�^AE�ABbA?�mA>VA<=qA;oA:^5A9��A9/A8z�A7p�A7VA6v�A5XA4$�A3��A3;dA2�9A/�mA/C�A,z�A,  A* �A(�yA&��A%K�A"r�A!��A!
=A|�A��AE�A�DA�^A�9A�
A�PA&�A�A��A�;A�yAz�A��Al�A��AĜAt�AjAbA�AbNAJA��A1AȴA
��A
�AoA1'A�jA�
AZA|�A%@���@�n�@��@���@�J@�O�@��9@��
@���@�^5@�|�@�p�@��@�h@��@��@��@�9@�@�Ĝ@�(�@柾@�-@�&�@�Q�@��
@�dZ@�K�@�33@��@�=q@ᙚ@��D@�1@�t�@���@�Z@۝�@��@�J@�X@��@���@�j@��
@֏\@ղ-@�hs@ԛ�@�l�@ҧ�@�O�@мj@Ͼw@��@�n�@��T@Ͳ-@�p�@�&�@�I�@�K�@ʟ�@�ff@�^5@�V@�M�@�5?@�@��#@ɉ7@���@ț�@�(�@��;@ǶF@�S�@�@���@Ɨ�@�M�@�O�@��`@��`@�bN@�  @Ý�@�S�@�"�@��@�&�@�&�@��h@���@��D@�1'@���@��@��@�G�@��@�1'@��@�@���@��m@�V@��#@��h@�bN@�(�@�j@�Z@���@�"�@�hs@�dZ@�"�@�C�@�l�@�t�@���@��F@��
@��m@��w@�|�@�ȴ@�7L@�Q�@��m@�\)@�K�@�+@��@��@���@�V@�$�@�{@�@��@��#@�@���@�O�@���@�I�@�ƨ@��F@�K�@�@�$�@�7L@�V@�/@���@��
@��9@��@�1'@�b@���@���@��@�~�@��^@���@�I�@� �@�b@���@��m@���@�;d@��R@��H@���@��y@��y@���@��@�~�@�@�5?@��@���@���@�{@�J@���@�hs@��@�r�@��@���@�
=@�~�@�=q@�@��@���@�/@��9@��D@�z�@�j@�(�@��@�ƨ@��@�33@��R@�n�@�M�@�@���@���@��h@�X@�%@��@�dZ@��@�C�@�@��@�M�@��^@��@�G�@�?}@��@��@���@��j@��D@�1@�ƨ@��@�S�@��y@��@���@��@�ȴ@���@��\@�^5@�M�@�-@�`B@���@�Z@�(�@���@�ƨ@���@�33@��@��!@�5?@��7@�/@���@�I�@�1'@�  @���@��@�S�@�C�@�
=@���@���@�v�@�-@�@��-@���@���@���@���@���@�p�@�O�@�%@���@��D@�bN@� �@���@��;@��F@��P@�l�@�dZ@���@��!@���@��\@�$�@���@��h@�p�@}�=11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	1'B	33B	33B	1'B	1'B	1'B	?}B	L�B	dZB	q�B	�VB	��B	�B	�wB	ĜB	ǮB	ȴB	�B
	7B
)�B
,B
1'B
B�B
K�B
K�B
o�B
�B
�B
}�B
�B
�BB�B;dBS�BZBm�B�{B�9B��B�B�!B�B�B��B��BĜB��BJB�B%�B"�B"�B%�B&�B'�B'�B0!B1'B9XB@�BJ�BK�BJ�BH�BK�BI�BH�BO�BI�B@�B9XB9XB0!B �B�BoB
=B  B�B��B�9B��B��B�+B}�Bl�B]/BE�B)�B�BVB
��B
�`B
�B
�B
�PB
v�B
bNB
P�B
;dB
(�B
bB	�B	�)B	��B	�XB	�B	�oB	w�B	e`B	y�B	�B	q�B	e`B	M�B	<jB	.B	�B	1B	  B��B��B�B�`B�;B��BÖB�RB�3B�'B�'B�'B�!B�B�B�B��B��B��B��B��B��B��B��B�VB��B�uB��B��B��B��B��B��B��B��B��B�B�9B�RB�RB�RB�LB�dB�}BŢBȴBȴBĜBȴBȴB��B��B��B��B��BȴBǮB��B��B�NB��B�B�sB�NB��B��B��BǮBĜBĜBĜBĜBÖBB��BBBÖB��B��B�qB�^B�dB�XB�LB�XB��B��B��BƨB��B��B�
B�B�#B�)B�5B�BB�HB�HB�NB�`B�mB�yB�yB�yB�B��B��B��B��B��B��B	  B	B	%B	DB	PB	VB	bB	{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	"�B	%�B	&�B	'�B	(�B	(�B	)�B	+B	-B	33B	5?B	6FB	7LB	:^B	=qB	>wB	>wB	B�B	D�B	J�B	J�B	J�B	K�B	N�B	P�B	R�B	S�B	YB	]/B	bNB	n�B	q�B	t�B	w�B	z�B	z�B	x�B	w�B	x�B	y�B	|�B	� B	�B	�uB	��B	��B	��B	��B	��B	��B	��B	�-B	�!B	�B	�B	�B	�-B	�RB	�^B	�jB	�}B	��B	ÖB	ƨB	ƨB	ƨB	ĜB	B	B	B	ŢB	ƨB	ǮB	ǮB	ǮB	ƨB	ƨB	ƨB	ƨB	ŢB	ŢB	ŢB	ŢB	ŢB	ĜB	ÖB	B	B	B	B	ŢB	ƨB	ǮB	ǮB	ȴB	��B	��B	�B	�B	�#B	�B	�B	�B	�B	�B	��B	��B	��B	�B	�B	�B	�B	�B	�)B	�5B	�;B	�BB	�NB	�TB	�TB	�NB	�ZB	�mB	�sB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B	��B	��B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
B
B
B
B
B
%B
%B
1B
1B
	7B

=B
	7B
	7B
1B
1B
1B

=B
DB
PB
VB
VB
VB
VB
\B
VB
\B
\B
\B
\B
\B
bB
bB
bB
bB
bB
bB
hB
hB
hB
hB
hB
hB
oB
oB
{B
{B
�B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
+k22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222   B	1'B	33B	33B	1'B	1'B	1'B	?}B	L�B	dZB	q�B	�VB	��B	�B	�wB	ĜB	ǮB	ȴB	�B
	7B
)�B
,B
1'B
B�B
K�B
K�B
o�B
�B
�B
}�B
�B
�BB�B;dBS�BZBm�B�{B�9B��B�B�!B�B�B��B��BĜB��BJB�B%�B"�B"�B%�B&�B'�B'�B0!B1'B9XB@�BJ�BK�BJ�BH�BK�BI�BH�BO�BI�B@�B9XB9XB0!B �B�BoB
=B  B�B��B�9B��B��B�+B}�Bl�B]/BE�B)�B�BVB
��B
�`B
�B
�B
�PB
v�B
bNB
P�B
;dB
(�B
bB	�B	�)B	��B	�XB	�B	�oB	w�B	e`B	y�B	�B	q�B	e`B	M�B	<jB	.B	�B	1B	  B��B��B�B�`B�;B��BÖB�RB�3B�'B�'B�'B�!B�B�B�B��B��B��B��B��B��B��B��B�VB��B�uB��B��B��B��B��B��B��B��B��B�B�9B�RB�RB�RB�LB�dB�}BŢBȴBȴBĜBȴBȴB��B��B��B��B��BȴBǮB��B��B�NB��B�B�sB�NB��B��B��BǮBĜBĜBĜBĜBÖBB��BBBÖB��B��B�qB�^B�dB�XB�LB�XB��B��B��BƨB��B��B�
B�B�#B�)B�5B�BB�HB�HB�NB�`B�mB�yB�yB�yB�B��B��B��B��B��B��B	  B	B	%B	DB	PB	VB	bB	{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	"�B	%�B	&�B	'�B	(�B	(�B	)�B	+B	-B	33B	5?B	6FB	7LB	:^B	=qB	>wB	>wB	B�B	D�B	J�B	J�B	J�B	K�B	N�B	P�B	R�B	S�B	YB	]/B	bNB	n�B	q�B	t�B	w�B	z�B	z�B	x�B	w�B	x�B	y�B	|�B	� B	�B	�uB	��B	��B	��B	��B	��B	��B	��B	�-B	�!B	�B	�B	�B	�-B	�RB	�^B	�jB	�}B	��B	ÖB	ƨB	ƨB	ƨB	ĜB	B	B	B	ŢB	ƨB	ǮB	ǮB	ǮB	ƨB	ƨB	ƨB	ƨB	ŢB	ŢB	ŢB	ŢB	ŢB	ĜB	ÖB	B	B	B	B	ŢB	ƨB	ǮB	ǮB	ȴB	��B	��B	�B	�B	�#B	�B	�B	�B	�B	�B	��B	��B	��B	�B	�B	�B	�B	�B	�)B	�5B	�;B	�BB	�NB	�TB	�TB	�NB	�ZB	�mB	�sB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B	��B	��B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
B
B
B
B
B
%B
%B
1B
1B
	7B

=B
	7B
	7B
1B
1B
1B

=B
DB
PB
VB
VB
VB
VB
\B
VB
\B
\B
\B
\B
\B
bB
bB
bB
bB
bB
bB
hB
hB
hB
hB
hB
hB
oB
oB
{B
{B
�B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
+k22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.31 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190556                              AO  ARCAADJP                                                                    20181005190556    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190556  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190556  QCF$                G�O�G�O�G�O�8000            