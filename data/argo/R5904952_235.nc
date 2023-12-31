CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:05:58Z creation      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181005190558  20181005190558  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @��)�81   @��*l�f@0�(�\�c�Ƨ1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @9��@�  @�  A   AffA@  A`  A�  A�  A�33A�  A�  A�33A�  A�  B   B  BffBffB ffB(  B0  B8  B@  BH  BP  BX  B`  Bh  BpffBxffB�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C�fC  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:�C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp�Cr  Ct  Cv  Cw�fCy�fC|  C~  C��C��C�  C�  C�  C�  C��C��C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C��3C�  C�  C��3C��3C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C��3C�  C�  C��C��C��C�  C�  C��3C�  C�  C�  C�  C��C��C��C��C�  C�  C�  C��3C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C��C��C�  C�  C�  C�  C�  C��3C�  C�  C��C��C��C�  C�  C�  C�  C��C�  C�  C�  C�  C��C�  C�  C��3C�  C�  C�  C��C�  C�  C��C�  C��3C�  C�  C�  C�  C�  C��C�  C�  C��3C��3C��3C��3C��3C��3C��3D   D � D ��Dy�D  D�fDfD� D  D� D��D� DfD�fD  D� D  D� D	  D	� D
  D
� D  D� D  D� D��D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  Dy�D��D� D  D� D  D� D  Dy�D��Dy�D  D� D��Dy�D  D� DfD�fD  D� D  D� D   D � D!  D!� D!��D"� D#  D#� D$  D$� D%  D%� D%��D&� D'  D'y�D'��D(� D)  D)�fD*fD*� D+  D+y�D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2y�D2��D3� D4  D4� D4��D5� D6fD6� D7  D7� D7��D8y�D8��D9� D:  D:� D:��D;y�D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� D@��DA� DB  DB� DB��DC� DDfDD� DE  DE� DF  DF� DGfDG�fDHfDH� DI  DI� DJ  DJ� DK  DK� DK��DLy�DM  DM� DN  DN�fDO  DO� DP  DP� DQ  DQ�fDR  DRy�DS  DS� DT  DTy�DU  DU� DV  DV� DW  DW� DW��DX� DY  DY� DZ  DZy�D[  D[� D\  D\y�D\��D]� D^fD^� D_  D_� D`fD`�fDa  Da� Db  Dby�Dc  Dc� Dd  Dd� De  De� Df  Dfy�Df��Dg� Dh  Dh� Di  Di� Dj  Djy�Dk  Dk�fDl  Dl� Dm  Dm� Dn  Dn� Do  Do� DpfDp�fDqfDq�fDr  Dr� Ds  Ds� Dt  Dt� Du  Du� Du��Dvy�Dw  Dw�fDw� Dy�\D�L�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @Mp�@��@��A��A#\)AD��Ad��A�z�A�z�A��A�z�A�z�AѮA�z�A�z�B=qB	=qB��B��B!��B)=qB1=qB9=qBA=qBI=qBQ=qBY=qBa=qBi=qBq��By��B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���BĞ�BȞ�B̞�BО�BԞ�B؞�Bܞ�B���B䞸B螸B잸B�B���B���B���C O\CO\CO\CO\CO\C
O\CO\C5�CO\CO\CO\CO\CO\CO\CO\CO\C O\C"O\C$O\C&O\C(O\C*O\C,O\C.O\C0O\C2O\C4O\C6O\C8O\C:h�C<O\C>O\C@O\CBO\CDO\CFO\CHO\CJO\CLO\CNO\CPO\CRO\CTO\CVO\CXO\CZO\C\O\C^O\C`O\CbO\CdO\CfO\ChO\CjO\ClO\CnO\Cph�CrO\CtO\CvO\Cx5�Cz5�C|O\C~O\C�4{C�4{C�'�C�'�C�'�C�'�C�4{C�4{C�'�C�'�C�'�C�4{C�4{C�'�C�'�C�'�C�'�C�'�C�'�C��C�'�C�'�C��C��C�'�C�'�C�'�C�'�C��C�'�C�'�C�'�C�'�C�'�C�'�C��C�'�C�'�C�4{C�4{C�4{C�'�C�'�C��C�'�C�'�C�'�C�'�C�4{C�4{C�4{C�4{C�'�C�'�C�'�C��C�'�C�'�C�'�C�'�C�'�C��C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C��C�'�C�4{C�4{C�4{C�'�C�'�C�'�C�'�C�'�C��C�'�C�'�C�4{C�4{C�4{C�'�C�'�C�'�C�'�C�4{C�'�C�'�C�'�C�'�C�4{C�'�C�'�C��C�'�C�'�C�'�C�4{C�'�C�'�C�4{C�'�C��C�'�C�'�C�'�C�'�C�'�C�4{C�'�C�'�C��C��C��C��C��C��C��D �D ��DqD�qD�D�=D=D��D�D��DqD��D=D�=D�D��D�D��D	�D	��D
�D
��D�D��D�D��DqD��D�D�qD�D��D�D��D�D��D�D��D�D��D�D�qDqD��D�D��D�D��D�D�qDqD�qD�D��DqD�qD�D��D=D�=D�D��D�D��D �D ��D!�D!��D"qD"��D#�D#��D$�D$��D%�D%��D&qD&��D'�D'�qD(qD(��D)�D)�=D*=D*��D+�D+�qD,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2�qD3qD3��D4�D4��D5qD5��D6=D6��D7�D7��D8qD8�qD9qD9��D:�D:��D;qD;�qD<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DAqDA��DB�DB��DCqDC��DD=DD��DE�DE��DF�DF��DG=DG�=DH=DH��DI�DI��DJ�DJ��DK�DK��DLqDL�qDM�DM��DN�DN�=DO�DO��DP�DP��DQ�DQ�=DR�DR�qDS�DS��DT�DT�qDU�DU��DV�DV��DW�DW��DXqDX��DY�DY��DZ�DZ�qD[�D[��D\�D\�qD]qD]��D^=D^��D_�D_��D`=D`�=Da�Da��Db�Db�qDc�Dc��Dd�Dd��De�De��Df�Df�qDgqDg��Dh�Dh��Di�Di��Dj�Dj�qDk�Dk�=Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp=Dp�=Dq=Dq�=Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��DvqDv�qDw�Dw�=Dw��Dy�3D�V�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�&�A�&�A�(�A�&�A�&�A�(�A�+A�-A�-A�/A�1'A�1'A�/A�1'A�1'A�1'A�1'A�5?A�;dA�=qA�?}A�E�A�G�A�I�A�1'A�%A��A���A���Aȡ�A�O�A�/A���AǋDA��
A�z�A���A� �A�{Aȕ�A�bNA���A�%A�z�A�^5A��A��A�ƨAç�A�=qA��A���A�I�A��hA�VA��jA�Q�A�$�A���A�;dA���A��HA���A�
=A�1A�$�A���A��A�O�A���A�ƨA��A�ffA�ƨA���A�=qA�C�A�z�A�O�A�+A�K�A�jA� �A�$�A��RA�VA��7A��A�E�A��A���A��A���A�l�A�M�A�ĜA�\)A���A�A��yA�7LA�"�A��^A}p�Av�RAt�uAs��AsVAq�hAm��AkAh��Ad�Ab��AbjAahsA_��A\�AW�mAS��AO�hAL�`AI�AD��AA�A@v�A=l�A9�A89XA7A7?}A57LA3�-A1�A0��A.~�A-�TA,��A,(�A+�A)�wA(n�A&��A%��A#��A!��A �\A A�A��A�Az�AM�A  A�-A�A�DA�
AVAI�AƨA�hAS�A33AȴA�wA�mA�A�HAz�A��AM�A��A7LAn�A�AS�Az�A�A��A~�A5?A�A�^A�/A��A�A��Az�A�A�A �A -@�|�@�?}@�S�@��@�7L@��9@�z�@��T@�33@홚@�@�V@���@�dZ@�R@�@�G�@�w@�X@���@�  @�J@�h@�1'@��
@��@�ff@�J@�x�@�&�@�1'@���@�=q@ٺ^@�%@أ�@�b@�ȴ@�n�@�v�@�Ĝ@�(�@ӝ�@�dZ@�dZ@�M�@ѡ�@���@�@�V@�|�@Η�@Χ�@�V@��@͑h@͉7@�X@��@�Ĝ@�A�@�b@˕�@�ff@�@���@ɲ-@ɑh@�p�@�&�@�r�@ư!@ź^@�Z@þw@ģ�@���@��/@��/@ēu@��@�;d@��@�M�@���@�X@���@��@���@�  @��
@���@�ƨ@��F@���@��@���@�S�@��R@�-@��@��@�z�@�r�@�1'@�b@�b@��@���@�;d@��@���@�=q@��-@�hs@�p�@�G�@��`@���@�j@�|�@�-@��^@���@��@�b@���@��@�"�@�V@�$�@���@��-@��^@���@�`B@��j@���@��D@��u@�z�@�r�@� �@��w@��w@��@�|�@�K�@�"�@���@���@�^5@��@��@��@�z�@��D@�A�@��m@��@���@�M�@�E�@�=q@�X@�X@�G�@�V@���@�(�@��F@��@�\)@��@�
=@�"�@�Q�@�z�@���@�C�@�33@���@�+@�^5@��@�&�@�Q�@�r�@��u@��D@�Q�@��
@�K�@���@���@�~�@�5?@��7@��D@���@��u@�Q�@���@��@���@���@���@��P@�dZ@�"�@��R@��@���@�p�@�O�@�&�@�j@��@��P@�\)@���@�^5@�$�@���@��@�%@���@���@���@�j@�1@���@���@�l�@��@�C�@��R@�5?@�{@���@���@�x�@��@��@��@��@�1'@��@���@���@�dZ@�@��@��@��@�V@�v�@�5?@��h@�`B@�&�@��@�1'@���@�1'@�b@�1@��F@���@�|�@�dZ@�C�@�@�v�@�^5@�V@�V@�$�@��-@�&�@���@��@��D@�Z@�1'@���@�K�@�"�@��@�
=@�
=@�@��y@���@��R@�V@��@��@���@��#@���@�x�@�&�@�%@�	�@u�D111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�&�A�&�A�(�A�&�A�&�A�(�A�+A�-A�-A�/A�1'A�1'A�/A�1'A�1'A�1'A�1'A�5?A�;dA�=qA�?}A�E�A�G�A�I�A�1'A�%A��A���A���Aȡ�A�O�A�/A���AǋDA��
A�z�A���A� �A�{Aȕ�A�bNA���A�%A�z�A�^5A��A��A�ƨAç�A�=qA��A���A�I�A��hA�VA��jA�Q�A�$�A���A�;dA���A��HA���A�
=A�1A�$�A���A��A�O�A���A�ƨA��A�ffA�ƨA���A�=qA�C�A�z�A�O�A�+A�K�A�jA� �A�$�A��RA�VA��7A��A�E�A��A���A��A���A�l�A�M�A�ĜA�\)A���A�A��yA�7LA�"�A��^A}p�Av�RAt�uAs��AsVAq�hAm��AkAh��Ad�Ab��AbjAahsA_��A\�AW�mAS��AO�hAL�`AI�AD��AA�A@v�A=l�A9�A89XA7A7?}A57LA3�-A1�A0��A.~�A-�TA,��A,(�A+�A)�wA(n�A&��A%��A#��A!��A �\A A�A��A�Az�AM�A  A�-A�A�DA�
AVAI�AƨA�hAS�A33AȴA�wA�mA�A�HAz�A��AM�A��A7LAn�A�AS�Az�A�A��A~�A5?A�A�^A�/A��A�A��Az�A�A�A �A -@�|�@�?}@�S�@��@�7L@��9@�z�@��T@�33@홚@�@�V@���@�dZ@�R@�@�G�@�w@�X@���@�  @�J@�h@�1'@��
@��@�ff@�J@�x�@�&�@�1'@���@�=q@ٺ^@�%@أ�@�b@�ȴ@�n�@�v�@�Ĝ@�(�@ӝ�@�dZ@�dZ@�M�@ѡ�@���@�@�V@�|�@Η�@Χ�@�V@��@͑h@͉7@�X@��@�Ĝ@�A�@�b@˕�@�ff@�@���@ɲ-@ɑh@�p�@�&�@�r�@ư!@ź^@�Z@þw@ģ�@���@��/@��/@ēu@��@�;d@��@�M�@���@�X@���@��@���@�  @��
@���@�ƨ@��F@���@��@���@�S�@��R@�-@��@��@�z�@�r�@�1'@�b@�b@��@���@�;d@��@���@�=q@��-@�hs@�p�@�G�@��`@���@�j@�|�@�-@��^@���@��@�b@���@��@�"�@�V@�$�@���@��-@��^@���@�`B@��j@���@��D@��u@�z�@�r�@� �@��w@��w@��@�|�@�K�@�"�@���@���@�^5@��@��@��@�z�@��D@�A�@��m@��@���@�M�@�E�@�=q@�X@�X@�G�@�V@���@�(�@��F@��@�\)@��@�
=@�"�@�Q�@�z�@���@�C�@�33@���@�+@�^5@��@�&�@�Q�@�r�@��u@��D@�Q�@��
@�K�@���@���@�~�@�5?@��7@��D@���@��u@�Q�@���@��@���@���@���@��P@�dZ@�"�@��R@��@���@�p�@�O�@�&�@�j@��@��P@�\)@���@�^5@�$�@���@��@�%@���@���@���@�j@�1@���@���@�l�@��@�C�@��R@�5?@�{@���@���@�x�@��@��@��@��@�1'@��@���@���@�dZ@�@��@��@��@�V@�v�@�5?@��h@�`B@�&�@��@�1'@���@�1'@�b@�1@��F@���@�|�@�dZ@�C�@�@�v�@�^5@�V@�V@�$�@��-@�&�@���@��@��D@�Z@�1'@���@�K�@�"�@��@�
=@�
=@�@��y@���@��R@�V@��@��@���@��#@���@�x�@�&�@�%@�	�@u�D111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Bl�Bl�Bm�Bl�Bl�Bl�Bl�Bm�Bl�Bl�Bm�Bm�Bl�Bl�Bl�Bl�Bm�Bn�Bs�Bs�By�B�B�1B�hB�9B��B�/B�B�B	B	"�B	/B	>wB	^5B	}�B	�jB
33B
\)B
�1B
�\B
�?B
��B�B<jBK�BffBq�B�\B��B�B�wB��B�yB��BB%B	7B�B'�B,B/B/B8RBA�BE�BH�BW
BVBXBXBS�BI�BB�B>wB5?B%�B�B�B
=B��B�ZBĜBB�LB��B�DB�Bu�Be`BQ�B.B�B\B
=B
��B
��B
��B
n�B
A�B
%�B
�B	��B	�HB	ÖB	��B	�PB	�1B	�B	u�B	aHB	M�B	>wB	,B	 �B	�B	{B	1B��B�NB��B�dB�'B��B��B��B��B�uB�uB�oB�hB�hB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�B�B�B�!B�!B�B�B�9B�qBBŢBƨBƨBÖBŢBƨBƨBɺB��B��BǮB�wB�qB��B�3B�B��B��B��B��B�qB�B�B�B�B�B�B��B��B��B��B��BǮBŢBɺB��B��B��B��B��BɺBɺBɺB��B��B��B�;B�HB�ZB�ZB�fB�fB�fB�mB�mB�B�B�B�B��B��B��B��B��B	B	B	B	B	B	%B	PB	bB	�B	�B	�B	�B	�B	!�B	$�B	'�B	(�B	,B	,B	-B	.B	1'B	49B	6FB	:^B	;dB	;dB	<jB	<jB	<jB	<jB	=qB	>wB	=qB	<jB	B�B	L�B	P�B	Q�B	R�B	VB	XB	XB	W
B	VB	T�B	T�B	ZB	]/B	_;B	dZB	dZB	e`B	e`B	e`B	gmB	k�B	n�B	u�B	w�B	z�B	}�B	�B	�B	�B	�=B	�PB	�bB	�hB	�oB	�{B	��B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�'B	�-B	�?B	�FB	�LB	�XB	�^B	�dB	�qB	�wB	��B	ĜB	ƨB	ŢB	ƨB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�
B	�
B	�
B	�
B	�B	�B	�B	�B	�#B	�TB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
  B
  B
  B
  B
  B
  B
  B
B
B
B
B
B
B
B
%B
+B
1B
	7B
DB
DB
DB
JB
JB
PB
PB
JB
DB
DB
JB
JB
VB
bB
bB
bB
\B
\B
\B
\B
bB
\B
bB
oB
hB
hB
oB
uB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
 �B
 �B
 �B
 �B
!�B
!�B
!�B
"�B
"�B
#�B
%�B
%�B
%�B
%�B
'�B
'B
9�222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  Bl�Bl�Bm�Bl�Bl�Bl�Bl�Bm�Bl�Bl�Bm�Bm�Bl�Bl�Bl�Bl�Bm�Bn�Bs�Bs�By�B�B�1B�hB�9B��B�/B�B�B	B	"�B	/B	>wB	^5B	}�B	�jB
33B
\)B
�1B
�\B
�?B
��B�B<jBK�BffBq�B�\B��B�B�wB��B�yB��BB%B	7B�B'�B,B/B/B8RBA�BE�BH�BW
BVBXBXBS�BI�BB�B>wB5?B%�B�B�B
=B��B�ZBĜBB�LB��B�DB�Bu�Be`BQ�B.B�B\B
=B
��B
��B
��B
n�B
A�B
%�B
�B	��B	�HB	ÖB	��B	�PB	�1B	�B	u�B	aHB	M�B	>wB	,B	 �B	�B	{B	1B��B�NB��B�dB�'B��B��B��B��B�uB�uB�oB�hB�hB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�B�B�B�!B�!B�B�B�9B�qBBŢBƨBƨBÖBŢBƨBƨBɺB��B��BǮB�wB�qB��B�3B�B��B��B��B��B�qB�B�B�B�B�B�B��B��B��B��B��BǮBŢBɺB��B��B��B��B��BɺBɺBɺB��B��B��B�;B�HB�ZB�ZB�fB�fB�fB�mB�mB�B�B�B�B��B��B��B��B��B	B	B	B	B	B	%B	PB	bB	�B	�B	�B	�B	�B	!�B	$�B	'�B	(�B	,B	,B	-B	.B	1'B	49B	6FB	:^B	;dB	;dB	<jB	<jB	<jB	<jB	=qB	>wB	=qB	<jB	B�B	L�B	P�B	Q�B	R�B	VB	XB	XB	W
B	VB	T�B	T�B	ZB	]/B	_;B	dZB	dZB	e`B	e`B	e`B	gmB	k�B	n�B	u�B	w�B	z�B	}�B	�B	�B	�B	�=B	�PB	�bB	�hB	�oB	�{B	��B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�'B	�-B	�?B	�FB	�LB	�XB	�^B	�dB	�qB	�wB	��B	ĜB	ƨB	ŢB	ƨB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�
B	�
B	�
B	�
B	�B	�B	�B	�B	�#B	�TB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
  B
  B
  B
  B
  B
  B
  B
B
B
B
B
B
B
B
%B
+B
1B
	7B
DB
DB
DB
JB
JB
PB
PB
JB
DB
DB
JB
JB
VB
bB
bB
bB
\B
\B
\B
\B
bB
\B
bB
oB
hB
hB
oB
uB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
 �B
 �B
 �B
 �B
!�B
!�B
!�B
"�B
"�B
#�B
%�B
%�B
%�B
%�B
'�B
'B
9�222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.31 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190558                              AO  ARCAADJP                                                                    20181005190558    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190558  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190558  QCF$                G�O�G�O�G�O�8000            