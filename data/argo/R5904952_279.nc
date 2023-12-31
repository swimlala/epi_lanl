CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:06:09Z creation      
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
_FillValue                 �  A8   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C,   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  T�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \x   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ^l   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  f4   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h(   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  o�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  w�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  y�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �t   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �h   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �0   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �`   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �`   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �`   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �`   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �    HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181005190609  20181005190609  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL              A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @��*(��m1   @��*���@2���+�c�E����1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                     A   A   A   @�ff@�33A   A   A@  A`  A�  A�  A�  A�  A�  A���A�  A�  B ffB  B��B  B   B(ffB0ffB7��B?��BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  C   C  C  C  C  C
  C  C  C  C  C�fC  C  C�fC  C  C   C"  C$  C&�C(�C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CO�fCR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C��3C�  C��C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C��C�  C�  C��C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C��C��C��C��C�  C��3C�  C��C��C�  C�  C�  C�  C�  C��3C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D�fD  Dy�D  D� D  D� D  D� D��Dy�D  D� D  D� D  D�fDfD� D  D� D  D� D  D� D  D� D  D� D��D� DfD�fDfD� D  D� D   D � D ��D!� D"  D"� D#  D#� D$  D$� D%fD%� D%��D&� D'fD'� D(  D(� D)  D)� D*  D*�fD+  D+� D,  D,� D,��D-� D.  D.� D/  D/�fD0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6y�D6��D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?y�D@  D@� DA  DA� DB  DB�fDCfDC� DD  DD�fDEfDE�fDF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ�fDK  DK� DL  DL�fDM  DM� DN  DN� DOfDO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT�fDUfDU�fDV  DV� DV��DWy�DX  DX�fDYfDY� DZ  DZy�D[  D[� D\  D\� D]  D]� D]��D^y�D^��D_y�D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De�fDf  Dfy�Dg  Dg�fDh  Dh� Di  Di� Dj  Dj� DkfDk�fDl  Dl� Dm  Dmy�Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt��Duy�Dv  Dv�fDwfDw�fDw�3Dy\D�=D��=111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�Q�@��A��A$��AD��Ad��A�z�A�z�A�z�A�z�A�z�A�G�A�z�A�z�B��B	=qB�B=qB!=qB)��B1��B8�B@�BI=qBQ=qBY=qBa=qBi=qBq=qBy=qB���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���BĞ�BȞ�B̞�BО�B�k�B؞�Bܞ�B���B䞸B螸B잸B�B���B���B���C O\CO\CO\CO\CO\C
O\CO\CO\CO\CO\C5�CO\CO\C5�CO\CO\C O\C"O\C$O\C&h�C(h�C*O\C,O\C.O\C0O\C2O\C4O\C6O\C8O\C:O\C<O\C>O\C@O\CBO\CDO\CFO\CHO\CJO\CLO\CNO\CP5�CRO\CTO\CVO\CXO\CZO\C\O\C^O\C`O\CbO\CdO\CfO\ChO\CjO\ClO\CnO\CpO\CrO\CtO\CvO\CxO\CzO\C|O\C~O\C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C��C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C��C��C�'�C�'�C�'�C��C�'�C�4{C�'�C�'�C��C�'�C�'�C�'�C�'�C�'�C�'�C�'�C��C�'�C�4{C�4{C�'�C�'�C�4{C�'�C�'�C�'�C�'�C��C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�4{C�'�C�'�C�'�C�4{C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C��C�'�C�4{C�4{C�4{C�4{C�4{C�'�C��C�'�C�4{C�4{C�'�C�'�C�'�C�'�C�'�C��C�'�C��C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�4{C�'�C�'�C�'�C�'�C�'�C�'�C�'�D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D�=D�D�qD�D��D�D��D�D��DqD�qD�D��D�D��D�D�=D=D��D�D��D�D��D�D��D�D��D�D��DqD��D=D�=D=D��D�D��D �D ��D!qD!��D"�D"��D#�D#��D$�D$��D%=D%��D&qD&��D'=D'��D(�D(��D)�D)��D*�D*�=D+�D+��D,�D,��D-qD-��D.�D.��D/�D/�=D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6�qD7qD7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?�qD@�D@��DA�DA��DB�DB�=DC=DC��DD�DD�=DE=DE�=DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ�=DK�DK��DL�DL�=DM�DM��DN�DN��DO=DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT�=DU=DU�=DV�DV��DWqDW�qDX�DX�=DY=DY��DZ�DZ�qD[�D[��D\�D\��D]�D]��D^qD^�qD_qD_�qD`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De�=Df�Df�qDg�Dg�=Dh�Dh��Di�Di��Dj�Dj��Dk=Dk�=Dl�Dl��Dm�Dm�qDn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��DuqDu�qDv�Dv�=Dw=Dw�=Dw�
Dy�3D�GD��)111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Aɥ�Aɧ�Aɧ�Aɰ!Aɩ�Aɧ�Aɧ�Aɧ�Aɩ�AɮAɴ9AɸRAɸRAɺ^AɸRAɸRAɬAɕ�A�t�A�?}A�oA�
=A�JA��#A��mA���AƩ�Aƴ9AƝ�A�z�A�v�A�33A���A�jA���A�v�A�VA�+A���A�A��A�  Aå�A�l�A�S�A�?}A��AuA��A���A�1'A�(�A��uA�5?A�&�A�-A� �A�oA���A��A��;A�`BA��A���A�S�A�%A�ĜA���A�%A�|�A�/A�JA��wA�`BA�I�A� �A�%A��`A��`A���A��jA��-A���A��PA�I�A��^A��7A�ZA�A�A���A�S�A��A��^A�1'A�ƨA�t�A��A��A��A�ȴA�7LA��A�C�A��uA�S�A��PA�ȴA��RA� �A�/A��A�bA�A�C�A���A�;dA{C�Aw�Ar�/Ap(�Anz�Ag��A`�DAZJAU��AP�`AL�RAKx�AJ�+AJ{AIƨAG33AC�^AA��A?�mA> �A=�
A=��A="�A<bA:ĜA9��A9A7A4��A4=qA2^5A1C�A0v�A.�`A.�A-�-A-?}A,jA+/A)�A({A'��A'��A'l�A&�A"�RA �9Al�A��AI�AoA�DA�A�An�A�yA�A�A��A^5A�AoA�uA�A��AA��A�`A �A(�A�;A�hA�\AVA�A�A
~�A	+AA9XAG�A�A"�A��AȴA33A �y@��@�M�@���@���@��#@��#@�X@�bN@��+@��\@��@�X@���@�1@�t�@�S�@�33@�M�@��@�A�@�=q@�bN@�Q�@�j@��@��H@��^@�j@땁@��@�G�@���@��@��@�9X@�S�@�~�@�h@�1'@�
=@�G�@�o@�@��@���@�C�@�5?@�$�@�$�@�{@ݑh@�7L@�X@�=q@�t�@�33@�
=@��H@��@އ+@�-@��T@�p�@�G�@�V@��@ڧ�@ٺ^@��@�Z@��
@ו�@�t�@ם�@��@�O�@Ϯ@�=q@�A�@�I�@̛�@�E�@�I�@�o@�K�@�V@ɡ�@�b@��m@��@�ȴ@�+@��j@�  @��!@��@���@��9@��u@���@�&�@��h@��@�J@�G�@��@��@�9X@��@�bN@��@�;d@���@�=q@���@�V@�Ĝ@�Ĝ@�/@���@��@��j@�Ĝ@��j@���@�+@�V@�?}@���@��9@�bN@��@��@�33@��!@�~�@�^5@��@���@���@��^@��^@�p�@�?}@�/@�7L@�X@�p�@��T@�ȴ@��R@�^5@���@�p�@�/@�%@��@��7@�V@�~�@�(�@��@�1'@�dZ@�-@�-@��#@�G�@�b@��@�bN@�j@�I�@�(�@���@�
=@�n�@��T@�&�@���@��@���@���@�@���@��7@��@�/@��@���@�X@�%@�z�@�ƨ@���@��
@��
@���@��F@�\)@��y@��@��#@�p�@�G�@�V@���@�j@�9X@���@�"�@��@���@��+@�5?@���@�G�@���@��D@�9X@��;@���@���@��P@�|�@�C�@���@��!@��R@���@���@���@��!@�ff@�J@��7@�x�@��@��/@�j@�Z@��w@��H@�^5@��@��-@���@��7@�p�@��@� �@��P@�|�@�l�@�K�@��y@�@��@���@�~�@�5?@�5?@��#@���@�`B@�&�@���@�?}@���@�`B@��@���@��@�dZ@�@���@�n�@�$�@��@���@��7@��@�Q�@��
@�+@�ȴ@���@�v�@��7@�hs@�X@�&�@��Y@�7@|111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  Aɥ�Aɧ�Aɧ�Aɰ!Aɩ�Aɧ�Aɧ�Aɧ�Aɩ�AɮAɴ9AɸRAɸRAɺ^AɸRAɸRAɬAɕ�A�t�A�?}A�oA�
=A�JA��#A��mA���AƩ�Aƴ9AƝ�A�z�A�v�A�33A���A�jA���A�v�A�VA�+A���A�A��A�  Aå�A�l�A�S�A�?}A��AuA��A���A�1'A�(�A��uA�5?A�&�A�-A� �A�oA���A��A��;A�`BA��A���A�S�A�%A�ĜA���A�%A�|�A�/A�JA��wA�`BA�I�A� �A�%A��`A��`A���A��jA��-A���A��PA�I�A��^A��7A�ZA�A�A���A�S�A��A��^A�1'A�ƨA�t�A��A��A��A�ȴA�7LA��A�C�A��uA�S�A��PA�ȴA��RA� �A�/A��A�bA�A�C�A���A�;dA{C�Aw�Ar�/Ap(�Anz�Ag��A`�DAZJAU��AP�`AL�RAKx�AJ�+AJ{AIƨAG33AC�^AA��A?�mA> �A=�
A=��A="�A<bA:ĜA9��A9A7A4��A4=qA2^5A1C�A0v�A.�`A.�A-�-A-?}A,jA+/A)�A({A'��A'��A'l�A&�A"�RA �9Al�A��AI�AoA�DA�A�An�A�yA�A�A��A^5A�AoA�uA�A��AA��A�`A �A(�A�;A�hA�\AVA�A�A
~�A	+AA9XAG�A�A"�A��AȴA33A �y@��@�M�@���@���@��#@��#@�X@�bN@��+@��\@��@�X@���@�1@�t�@�S�@�33@�M�@��@�A�@�=q@�bN@�Q�@�j@��@��H@��^@�j@땁@��@�G�@���@��@��@�9X@�S�@�~�@�h@�1'@�
=@�G�@�o@�@��@���@�C�@�5?@�$�@�$�@�{@ݑh@�7L@�X@�=q@�t�@�33@�
=@��H@��@އ+@�-@��T@�p�@�G�@�V@��@ڧ�@ٺ^@��@�Z@��
@ו�@�t�@ם�@��@�O�@Ϯ@�=q@�A�@�I�@̛�@�E�@�I�@�o@�K�@�V@ɡ�@�b@��m@��@�ȴ@�+@��j@�  @��!@��@���@��9@��u@���@�&�@��h@��@�J@�G�@��@��@�9X@��@�bN@��@�;d@���@�=q@���@�V@�Ĝ@�Ĝ@�/@���@��@��j@�Ĝ@��j@���@�+@�V@�?}@���@��9@�bN@��@��@�33@��!@�~�@�^5@��@���@���@��^@��^@�p�@�?}@�/@�7L@�X@�p�@��T@�ȴ@��R@�^5@���@�p�@�/@�%@��@��7@�V@�~�@�(�@��@�1'@�dZ@�-@�-@��#@�G�@�b@��@�bN@�j@�I�@�(�@���@�
=@�n�@��T@�&�@���@��@���@���@�@���@��7@��@�/@��@���@�X@�%@�z�@�ƨ@���@��
@��
@���@��F@�\)@��y@��@��#@�p�@�G�@�V@���@�j@�9X@���@�"�@��@���@��+@�5?@���@�G�@���@��D@�9X@��;@���@���@��P@�|�@�C�@���@��!@��R@���@���@���@��!@�ff@�J@��7@�x�@��@��/@�j@�Z@��w@��H@�^5@��@��-@���@��7@�p�@��@� �@��P@�|�@�l�@�K�@��y@�@��@���@�~�@�5?@�5?@��#@���@�`B@�&�@���@�?}@���@�`B@��@���@��@�dZ@�@���@�n�@�$�@��@���@��7@��@�Q�@��
@�+@�ȴ@���@�v�@��7@�hs@�X@�&�@��Y@�7@|111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�5B�fB�B	VB	.B	=qB	W
B	��B
 �B
�B
�
B
�sB
�sB
�BBB	7B�B&�B6FB;dB>wB?}BC�BVBo�Bx�Bq�Bm�BjBe`BgmBu�B~�B�Bk�BbNBo�Bu�By�B{�B� B�B��B��B�jB�B!�B(�B1'B8RB=qBK�BVB\)B^5BcTBk�Bm�Bp�Bt�B{�B}�B�B�B�B� B~�B�B�B�B�B�B� B� B�DB�B�1B{�BaHBL�B-B�B��B�mBǮB�B�\BjB49BDB
��B
�BB
�LB
��B
�%B
dZB
9XB	��B	�jB	�7B	e`B	E�B	8RB	-B	&�B	uB��B�TB�
B��B��B��B��BɺBŢBĜBBÖBŢBƨBŢBŢBƨBȴBȴBȴB��B��B��B��BɺBǮBɺB��B��B��B��B��B��B��B��B��B��BǮBɺBƨBƨBȴB��B��B��B��B�
B�5B�#B��B��B�B�#B�;B�sB�B��B��B��B��B	B	%B		7B	PB	$�B	>wB	E�B	D�B	E�B	B�B	:^B	:^B	?}B	8RB	,B	%�B	7LB	8RB	1'B	0!B	0!B	1'B	2-B	49B	5?B	6FB	6FB	=qB	=qB	A�B	C�B	D�B	D�B	C�B	E�B	E�B	F�B	J�B	K�B	L�B	L�B	Q�B	S�B	YB	ZB	W
B	XB	XB	XB	dZB	n�B	jB	o�B	q�B	r�B	r�B	p�B	l�B	iyB	gmB	l�B	ffB	bNB	aHB	_;B	\)B	\)B	]/B	`BB	bNB	jB	o�B	v�B	�B	�bB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�'B	�-B	�9B	�XB	�dB	�9B	��B	��B	��B	��B	��B	��B	�B	�wB	�dB	�3B	�B	��B	��B	��B	�{B	�VB	� B	u�B	q�B	l�B	jB	hsB	e`B	ffB	jB	l�B	o�B	t�B	u�B	s�B	v�B	v�B	t�B	w�B	y�B	y�B	v�B	q�B	t�B	t�B	t�B	u�B	w�B	{�B	�B	�%B	�+B	�1B	�DB	�%B	�1B	�{B	��B	��B	��B	��B	��B	�{B	�uB	�uB	�uB	�uB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�'B	�!B	�'B	�9B	�dB	��B	�B	�NB	�fB	�`B	�`B	�ZB	�ZB	�ZB	�TB	�TB	�`B	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�yB	�mB	�`B	�ZB	�`B	�`B	�fB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
  B
  B
  B
  B	��B	��B	��B	��B	��B	��B	��B
B
%B
+B
+B
1B
	7B
+B
%B
B
B
B
	7B
JB
DB
DB

=B
	7B
	7B
	7B
	7B

=B
	7B
	7B
	7B
	7B
1B
1B
+B
+B
+B
+B
1B
	7B
	7B
	7B
JB
DB
�B
#�222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�5B�fB�B	VB	.B	=qB	W
B	��B
 �B
�B
�
B
�sB
�sB
�BBB	7B�B&�B6FB;dB>wB?}BC�BVBo�Bx�Bq�Bm�BjBe`BgmBu�B~�B�Bk�BbNBo�Bu�By�B{�B� B�B��B��B�jB�B!�B(�B1'B8RB=qBK�BVB\)B^5BcTBk�Bm�Bp�Bt�B{�B}�B�B�B�B� B~�B�B�B�B�B�B� B� B�DB�B�1B{�BaHBL�B-B�B��B�mBǮB�B�\BjB49BDB
��B
�BB
�LB
��B
�%B
dZB
9XB	��B	�jB	�7B	e`B	E�B	8RB	-B	&�B	uB��B�TB�
B��B��B��B��BɺBŢBĜBBÖBŢBƨBŢBŢBƨBȴBȴBȴB��B��B��B��BɺBǮBɺB��B��B��B��B��B��B��B��B��B��BǮBɺBƨBƨBȴB��B��B��B��B�
B�5B�#B��B��B�B�#B�;B�sB�B��B��B��B��B	B	%B		7B	PB	$�B	>wB	E�B	D�B	E�B	B�B	:^B	:^B	?}B	8RB	,B	%�B	7LB	8RB	1'B	0!B	0!B	1'B	2-B	49B	5?B	6FB	6FB	=qB	=qB	A�B	C�B	D�B	D�B	C�B	E�B	E�B	F�B	J�B	K�B	L�B	L�B	Q�B	S�B	YB	ZB	W
B	XB	XB	XB	dZB	n�B	jB	o�B	q�B	r�B	r�B	p�B	l�B	iyB	gmB	l�B	ffB	bNB	aHB	_;B	\)B	\)B	]/B	`BB	bNB	jB	o�B	v�B	�B	�bB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�'B	�-B	�9B	�XB	�dB	�9B	��B	��B	��B	��B	��B	��B	�B	�wB	�dB	�3B	�B	��B	��B	��B	�{B	�VB	� B	u�B	q�B	l�B	jB	hsB	e`B	ffB	jB	l�B	o�B	t�B	u�B	s�B	v�B	v�B	t�B	w�B	y�B	y�B	v�B	q�B	t�B	t�B	t�B	u�B	w�B	{�B	�B	�%B	�+B	�1B	�DB	�%B	�1B	�{B	��B	��B	��B	��B	��B	�{B	�uB	�uB	�uB	�uB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�'B	�!B	�'B	�9B	�dB	��B	�B	�NB	�fB	�`B	�`B	�ZB	�ZB	�ZB	�TB	�TB	�`B	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�yB	�mB	�`B	�ZB	�`B	�`B	�fB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
  B
  B
  B
  B	��B	��B	��B	��B	��B	��B	��B
B
%B
+B
+B
1B
	7B
+B
%B
B
B
B
	7B
JB
DB
DB

=B
	7B
	7B
	7B
	7B

=B
	7B
	7B
	7B
	7B
1B
1B
+B
+B
+B
+B
1B
	7B
	7B
	7B
JB
DB
�B
#�222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.31 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190609                              AO  ARCAADJP                                                                    20181005190609    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190609  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190609  QCF$                G�O�G�O�G�O�8000            