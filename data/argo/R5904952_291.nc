CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:06:12Z creation      
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181005190612  20181005190612  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL              #A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @�)����1   @�*DDY0@1`A�7K��c�M���1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                     #A   A   A   @�33@�  A   A   A@  A`  A�  A���A�  A�  A�33A�  A�  A�  B   B  B  B  B   B(  B0  B7��B?��BH  BP  BX  B`ffBh  Bp  Bw��B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�33B�33B�  B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C�fC  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cq�fCs�fCv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C��C��C��C��C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C��C�  C��3C�  C�  C�  C��3C�  C�  C�  C�  C��C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C��3C�  C��C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D��D� D  Dy�D  D� D  D� D��D� D  D� D  D� D��D	y�D
  D
� D
��D� D  D� D  D� D  D� D  Dy�D  D�fD  D� D  D� D  D� D  D� D  D� D  Dy�D  D�fD  D� D  D� D  D� D  D� D��Dy�D  D� D  D� D  Dy�D   D � D ��D!� D"  D"� D#  D#�fD$fD$�fD%  D%y�D&  D&� D'  D'� D(  D(�fD)fD)�fD*fD*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D1��D2� D3fD3�fD4  D4� D5  D5�fD6  D6� D7  D7� D8  D8� D9  D9� D:  D:y�D:��D;y�D<  D<� D<��D=y�D=��D>y�D?  D?� D?��D@� DA  DA� DA��DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DGy�DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DMfDM� DN  DN� DOfDO� DPfDP� DP��DQy�DR  DR� DS  DSy�DS��DTy�DT��DUy�DV  DV� DW  DW� DX  DX� DX��DY� DZ  DZ� D[  D[� D\fD\�fD]fD]�fD^fD^� D^��D_�fD`  D`�fDafDay�Da��Db� Dc  Dc�fDd  Ddy�De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj�fDkfDky�Dl  Dl� Dm  Dmy�Dn  Dn� Do  Do�fDpfDp� Dq  Dq�fDr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dvy�Dw  Dw� Dw��Dy�D�<{11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��A��A$��AD��Ad��A�z�A�G�A�z�A�z�A��A�z�A�z�A�z�B=qB	=qB=qB=qB!=qB)=qB1=qB8�B@�BI=qBQ=qBY=qBa��Bi=qBq=qBx�B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���BĞ�BȞ�B�k�B�k�BԞ�B؞�Bܞ�B���B䞸B螸B잸B�B���B���B���C O\CO\CO\CO\CO\C
O\CO\CO\CO\CO\CO\CO\C5�CO\CO\CO\C O\C"O\C$O\C&O\C(O\C*O\C,O\C.O\C0O\C2O\C4O\C6O\C8O\C:O\C<O\C>O\C@O\CBO\CDO\CFO\CHO\CJO\CLO\CNO\CPO\CRO\CTO\CVO\CXO\CZO\C\O\C^O\C`O\CbO\CdO\CfO\ChO\CjO\ClO\CnO\CpO\Cr5�Ct5�CvO\CxO\CzO\C|O\C~O\C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C��C�'�C�'�C�'�C�'�C�'�C�4{C�4{C�'�C�'�C�'�C�'�C�'�C�'�C�4{C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C��C�'�C�4{C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C��C�'�C�'�C�'�C�'�C�4{C�4{C�4{C�4{C�'�C�'�C�'�C�'�C�'�C�'�C��C�'�C�'�C�'�C�'�C�'�C�'�C�4{C�'�C��C�'�C�'�C�'�C��C�'�C�'�C�'�C�'�C�4{C�'�C�4{C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C��C��C�'�C�'�C�'�C�'�C��C�'�C�4{C�'�C�'�C�'�C�'�C�'�C�'�C�'�D �D ��D�D��DqD��D�D�qD�D��D�D��DqD��D�D��D�D��D	qD	�qD
�D
��DqD��D�D��D�D��D�D��D�D�qD�D�=D�D��D�D��D�D��D�D��D�D��D�D�qD�D�=D�D��D�D��D�D��D�D��DqD�qD�D��D�D��D�D�qD �D ��D!qD!��D"�D"��D#�D#�=D$=D$�=D%�D%�qD&�D&��D'�D'��D(�D(�=D)=D)�=D*=D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2qD2��D3=D3�=D4�D4��D5�D5�=D6�D6��D7�D7��D8�D8��D9�D9��D:�D:�qD;qD;�qD<�D<��D=qD=�qD>qD>�qD?�D?��D@qD@��DA�DA��DBqDB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG�qDH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM=DM��DN�DN��DO=DO��DP=DP��DQqDQ�qDR�DR��DS�DS�qDTqDT�qDUqDU�qDV�DV��DW�DW��DX�DX��DYqDY��DZ�DZ��D[�D[��D\=D\�=D]=D]�=D^=D^��D_qD_�=D`�D`�=Da=Da�qDbqDb��Dc�Dc�=Dd�Dd�qDe�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj�=Dk=Dk�qDl�Dl��Dm�Dm�qDn�Dn��Do�Do�=Dp=Dp��Dq�Dq�=Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv�qDw�Dw��Dx �Dy��D�Fg11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�5?A�5?A�5?A�33A�1'A�1'A�1'A�/A�-A�+A�/A� �A�JA��yAˡ�A�XA�E�A�7LA���A���AʶFAʮAʥ�Aʣ�Aʝ�Aʙ�AʅA�`BA�1'A�JA��mA���A���A���A���Aɺ^Aɴ9Aə�A�9XA���Aȇ+A�&�AǃA�\)A�oA���Aŏ\A�1'A��#Aĥ�A�/AÛ�A§�A�hsA�I�A�-A�;dA�`BA��A���A���A��FA�ffA��7A���A�I�A��jA�7LA�n�A���A��^A�E�A��9A�=qA���A�/A��A�dZA�^5A�ĜA���A�&�A�bA�
=A�?}A��A��jA���A�K�A��jA��^A�l�A�oA�G�A��A�;dA��A�5?A�/A��A���A�I�A�33A��+A�1'A�A�JA�C�A�9XA��\A���A�t�Az��Av$�Aq"�An��AhJAdĜAc"�AaC�A^jA\r�A[oAY"�AW�mAVz�AQ��AN-AI��AH �ADȴAB~�A@�yA@�A?��A??}A??}A=�PA:�/A7��A6$�A5�A2�A1��A0��A/�PA.^5A-�A,�HA,=qA+%A)G�A(��A(��A'��A'|�A'�A'G�A&E�A%dZA$�RA"=qA ��A �A?}Ax�A��A�AffA=qA�PA��A�mAx�A��Av�A�AZA��A�AƨA��AK�AVAS�A�A��A�
A+A�A�RA�AVA��AI�AAx�A��Az�A�;A�wA�/A�mA�\AƨA?}A ĜA �+A r�A A�@���@��@�x�@��9@��u@�(�@��@��@���@�G�@�^5@�E�@��@��j@�&�@��;@��
@�@���@�%@�Q�@�|�@���@���@��T@�G�@�@��@��`@�@畁@��
@��@� �@�\)@�l�@�1'@�r�@�Q�@�b@�P@�
=@柾@�7@���@�P@��@��/@�I�@�A�@��@��/@���@��@�9X@� �@�;d@�v�@�J@��T@�`B@���@�z�@�A�@۾w@���@�v�@�n�@ڧ�@ڸR@���@ڟ�@�=q@١�@�O�@�V@���@�bN@��;@�;d@֧�@�@�X@��@�r�@���@��@ԃ@ԣ�@Ԭ@�Q�@�\)@�^5@�x�@Ѳ-@��#@Ѻ^@�O�@�9X@�K�@���@�@�X@�/@�%@���@�%@��`@̋D@��@˅@ʰ!@�n�@Ɂ@�Z@��m@�+@��y@���@�~�@�{@�`B@�%@�Ĝ@�bN@�1@Ý�@�"�@¸R@�M�@��@���@�7L@���@�j@�(�@��m@��w@��@�ȴ@��+@��@��^@�p�@��@��9@��@�t�@��y@�ff@�5?@���@��7@�7L@���@��9@��@��
@�|�@�K�@�"�@�ȴ@�v�@�{@��^@���@��@�G�@���@�z�@�b@���@�;d@��y@�M�@�?}@�1'@���@���@��@�\)@�v�@�5?@�~�@�=q@���@���@���@�%@�r�@�Q�@�A�@�  @���@�K�@�33@�@��!@�~�@�^5@�5?@���@��^@�%@��9@��u@�j@�1@��;@��F@���@��@�
=@�M�@��-@�p�@�7L@�&�@��@��9@��@��u@��D@�I�@�  @���@�S�@�+@��R@�n�@�E�@��T@�?}@��@��9@���@��D@�z�@�I�@�1@���@�v�@�^5@�M�@�E�@�5?@�-@��@��@��h@���@�9X@��;@���@�\)@�;d@�"�@���@�=q@�@�hs@��@�Z@�(�@�1@�  @��;@�o@��@���@��h@��h@�X@���@�Z@��@��
@�ƨ@�K�@���@�5?@���@�?}@���@��j@�7�@�o11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�5?A�5?A�5?A�33A�1'A�1'A�1'A�/A�-A�+A�/A� �A�JA��yAˡ�A�XA�E�A�7LA���A���AʶFAʮAʥ�Aʣ�Aʝ�Aʙ�AʅA�`BA�1'A�JA��mA���A���A���A���Aɺ^Aɴ9Aə�A�9XA���Aȇ+A�&�AǃA�\)A�oA���Aŏ\A�1'A��#Aĥ�A�/AÛ�A§�A�hsA�I�A�-A�;dA�`BA��A���A���A��FA�ffA��7A���A�I�A��jA�7LA�n�A���A��^A�E�A��9A�=qA���A�/A��A�dZA�^5A�ĜA���A�&�A�bA�
=A�?}A��A��jA���A�K�A��jA��^A�l�A�oA�G�A��A�;dA��A�5?A�/A��A���A�I�A�33A��+A�1'A�A�JA�C�A�9XA��\A���A�t�Az��Av$�Aq"�An��AhJAdĜAc"�AaC�A^jA\r�A[oAY"�AW�mAVz�AQ��AN-AI��AH �ADȴAB~�A@�yA@�A?��A??}A??}A=�PA:�/A7��A6$�A5�A2�A1��A0��A/�PA.^5A-�A,�HA,=qA+%A)G�A(��A(��A'��A'|�A'�A'G�A&E�A%dZA$�RA"=qA ��A �A?}Ax�A��A�AffA=qA�PA��A�mAx�A��Av�A�AZA��A�AƨA��AK�AVAS�A�A��A�
A+A�A�RA�AVA��AI�AAx�A��Az�A�;A�wA�/A�mA�\AƨA?}A ĜA �+A r�A A�@���@��@�x�@��9@��u@�(�@��@��@���@�G�@�^5@�E�@��@��j@�&�@��;@��
@�@���@�%@�Q�@�|�@���@���@��T@�G�@�@��@��`@�@畁@��
@��@� �@�\)@�l�@�1'@�r�@�Q�@�b@�P@�
=@柾@�7@���@�P@��@��/@�I�@�A�@��@��/@���@��@�9X@� �@�;d@�v�@�J@��T@�`B@���@�z�@�A�@۾w@���@�v�@�n�@ڧ�@ڸR@���@ڟ�@�=q@١�@�O�@�V@���@�bN@��;@�;d@֧�@�@�X@��@�r�@���@��@ԃ@ԣ�@Ԭ@�Q�@�\)@�^5@�x�@Ѳ-@��#@Ѻ^@�O�@�9X@�K�@���@�@�X@�/@�%@���@�%@��`@̋D@��@˅@ʰ!@�n�@Ɂ@�Z@��m@�+@��y@���@�~�@�{@�`B@�%@�Ĝ@�bN@�1@Ý�@�"�@¸R@�M�@��@���@�7L@���@�j@�(�@��m@��w@��@�ȴ@��+@��@��^@�p�@��@��9@��@�t�@��y@�ff@�5?@���@��7@�7L@���@��9@��@��
@�|�@�K�@�"�@�ȴ@�v�@�{@��^@���@��@�G�@���@�z�@�b@���@�;d@��y@�M�@�?}@�1'@���@���@��@�\)@�v�@�5?@�~�@�=q@���@���@���@�%@�r�@�Q�@�A�@�  @���@�K�@�33@�@��!@�~�@�^5@�5?@���@��^@�%@��9@��u@�j@�1@��;@��F@���@��@�
=@�M�@��-@�p�@�7L@�&�@��@��9@��@��u@��D@�I�@�  @���@�S�@�+@��R@�n�@�E�@��T@�?}@��@��9@���@��D@�z�@�I�@�1@���@�v�@�^5@�M�@�E�@�5?@�-@��@��@��h@���@�9X@��;@���@�\)@�;d@�"�@���@�=q@�@�hs@��@�Z@�(�@�1@�  @��;@�o@��@���@��h@��h@�X@���@�Z@��@��
@�ƨ@�K�@���@�5?@���@�?}@���@��j@�7�@�o11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�!B	�^B	��B	�BB	�HB	�HB	�/B	�B	�B	�/B	�)B	�)B	�#B	�#B	�;B	�B

=B
�B
(�B
8RB
;dB
=qB
?}B
@�B
A�B
G�B
hsB
�+B
��B
B
�BB+B
=B�B&�B2-B7LB;dBG�BS�Bo�Bs�Bs�Bs�By�B�7B��B��B�B�3B�}B��B�NBB�B�B�B49B-B0!B5?B5?B7LB8RB=qB;dB;dB49B2-B-B+B�BPBJBDBB�B��B�3B��B��B��B�PBw�BVBJ�B6FB�B
��B
�B
�B
�fB
�)B
��B
�LB
��B
�PB
t�B
J�B
0!B	��B	ȴB	��B	�oB	m�B	G�B	gmB	VB	L�B	?}B	5?B	1'B	&�B	�B	  B�BBÖB�RB�LB�B�B�B�B�B�XB�jB��B�?B�^B�}B�jB�LB�LB�LB�XB�LB�FB�9B�^B�FB�9B�?B�qB�qB��B��B��B��B��B��B��B��B��B��B��B��B�B�BB�ZB�BB�5B�;B�`B�B�B�sB�fB�TB�TB�yB�sB�B	B	\B	bB	�B	hB	VB	JB	
=B	JB	uB	�B	 �B	�B	B��B��B	  B��B	B	B��B	B		7B	VB	bB	hB	oB	�B	�B	�B	 �B	&�B	)�B	)�B	-B	0!B	=qB	B�B	E�B	C�B	K�B	J�B	=qB	:^B	8RB	9XB	=qB	A�B	D�B	D�B	G�B	L�B	I�B	B�B	I�B	L�B	R�B	]/B	e`B	o�B	m�B	q�B	y�B	|�B	~�B	� B	�B	�B	�B	�B	� B	}�B	�B	�B	� B	�B	�PB	�bB	�oB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�?B	�LB	�LB	�LB	�^B	�jB	�qB	�}B	��B	B	ÖB	ĜB	ĜB	ĜB	ÖB	ÖB	ĜB	ĜB	ƨB	��B	��B	��B	��B	ǮB	ƨB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�B	�#B	�/B	�5B	�5B	�5B	�HB	�TB	�ZB	�mB	�sB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
+B
1B
1B
1B
1B
1B
1B
1B
1B
1B
1B
1B
1B
1B
1B
1B
1B
1B
+B
+B
1B
1B
1B
	7B
1B
	7B
	7B
	7B

=B

=B

=B
	7B
	7B
	7B
DB
DB
DB
DB
DB
DB
DB
DB
DB
DB

=B

=B

=B

=B
DB

=B

=B

=B

=B
	7B
+B
%B
1B
1B
1B
	7B

=B

=B
	7B
	7B
	7B
	7B
	7B

=B
	7B
	7B

=B

=B

=B
JB
PB
JB
PB
PB
VB
JB
!�22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222   B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�!B	�^B	��B	�BB	�HB	�HB	�/B	�B	�B	�/B	�)B	�)B	�#B	�#B	�;B	�B

=B
�B
(�B
8RB
;dB
=qB
?}B
@�B
A�B
G�B
hsB
�+B
��B
B
�BB+B
=B�B&�B2-B7LB;dBG�BS�Bo�Bs�Bs�Bs�By�B�7B��B��B�B�3B�}B��B�NBB�B�B�B49B-B0!B5?B5?B7LB8RB=qB;dB;dB49B2-B-B+B�BPBJBDBB�B��B�3B��B��B��B�PBw�BVBJ�B6FB�B
��B
�B
�B
�fB
�)B
��B
�LB
��B
�PB
t�B
J�B
0!B	��B	ȴB	��B	�oB	m�B	G�B	gmB	VB	L�B	?}B	5?B	1'B	&�B	�B	  B�BBÖB�RB�LB�B�B�B�B�B�XB�jB��B�?B�^B�}B�jB�LB�LB�LB�XB�LB�FB�9B�^B�FB�9B�?B�qB�qB��B��B��B��B��B��B��B��B��B��B��B��B�B�BB�ZB�BB�5B�;B�`B�B�B�sB�fB�TB�TB�yB�sB�B	B	\B	bB	�B	hB	VB	JB	
=B	JB	uB	�B	 �B	�B	B��B��B	  B��B	B	B��B	B		7B	VB	bB	hB	oB	�B	�B	�B	 �B	&�B	)�B	)�B	-B	0!B	=qB	B�B	E�B	C�B	K�B	J�B	=qB	:^B	8RB	9XB	=qB	A�B	D�B	D�B	G�B	L�B	I�B	B�B	I�B	L�B	R�B	]/B	e`B	o�B	m�B	q�B	y�B	|�B	~�B	� B	�B	�B	�B	�B	� B	}�B	�B	�B	� B	�B	�PB	�bB	�oB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�?B	�LB	�LB	�LB	�^B	�jB	�qB	�}B	��B	B	ÖB	ĜB	ĜB	ĜB	ÖB	ÖB	ĜB	ĜB	ƨB	��B	��B	��B	��B	ǮB	ƨB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�B	�#B	�/B	�5B	�5B	�5B	�HB	�TB	�ZB	�mB	�sB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
+B
1B
1B
1B
1B
1B
1B
1B
1B
1B
1B
1B
1B
1B
1B
1B
1B
1B
+B
+B
1B
1B
1B
	7B
1B
	7B
	7B
	7B

=B

=B

=B
	7B
	7B
	7B
DB
DB
DB
DB
DB
DB
DB
DB
DB
DB

=B

=B

=B

=B
DB

=B

=B

=B

=B
	7B
+B
%B
1B
1B
1B
	7B

=B

=B
	7B
	7B
	7B
	7B
	7B

=B
	7B
	7B

=B

=B

=B
JB
PB
JB
PB
PB
VB
JB
!�22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.31 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190612                              AO  ARCAADJP                                                                    20181005190612    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190612  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190612  QCF$                G�O�G�O�G�O�8000            