CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2017-06-01T17:02:24Z creation      
references        (http://www.argodatamgt.org/Documentation   comment           user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
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
resolution        >�E�vQ�   
_FillValue        A.�~       axis      T           80   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    88   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�E�vQ�   
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
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  `�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  pL   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �|   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �(   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �l   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �X   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �4   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �4   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �4   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �4   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �`   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �d   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �h   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �l   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �p   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��        ��Argo profile    3.1 1.2 19500101000000  20170601170224  20181103100347  5904055 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               wA   AO  5004                            2C  D   NAVIS_A                         0303                            082713                          863 @�؃�g�1   @��  v@6pbM���e �\)1   GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                      wA   A   A   @���@���A   AffA@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0ffB7��B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C+�fC.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,fD,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D�|�D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D���D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D��f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�
>@�=q@�p�A�A>�RA^�RA~�RA�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)B�B�B�B�B'�B0zB7G�B?�BG�BO�BW�B_�Bg�Bo�Bw�B�B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
C�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+��C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D z�D ��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��D	z�D	��D
z�D
��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��D z�D ��D!z�D!��D"z�D"��D#z�D#��D$z�D$��D%z�D%��D&z�D&��D'z�D'��D(z�D(��D)z�D)��D*z�D*��D+z�D,GD,z�D,��D-z�D-��D.z�D.��D/z�D/��D0z�D0��D1z�D1��D2z�D2��D3z�D3��D4z�D4��D5z�D5��D6z�D6��D7z�D7��D8z�D8��D9z�D9��D:z�D:��D;z�D;��D<z�D<��D=z�D=��D>z�D>��D?z�D?��D@z�D@��DAz�DA��DBz�DB��DCz�DC��DDz�DD��DEz�DE��DFz�DF��DGz�DG��DHz�DH��DIz�DI��DJz�DJ��DKz�DK��DLz�DL��DMz�DM��DNz�DN��DOz�DO��DPz�DP��DQz�DQ��DRz�DR��DSz�DS��DTz�DT��DUz�DU��DVz�DV��DWz�DW��DXz�DX��DYz�DY��DZz�DZ��D[z�D[��D\z�D\��D]z�D]��D^z�D^��D_z�D_��D`z�D`��Daz�Da��Dbz�Db��Dcz�Dc��Ddz�Dd��Dez�De��Dfz�Df��Dgz�Dg��Dhz�Dh��Diz�Di��Djz�Dj��Dkz�Dk��Dlz�Dl��Dmz�Dm��Dnz�Dn��Doz�Do��Dpz�Dp��Dqz�Dq��Drz�Dr��Dsz�Ds��Dtz�Dt��Duz�Du��Dvz�Dv��Dwz�Dw��Dxz�Dx��Dyz�Dy��Dzz�Dz��D{z�D{��D|z�D|��D}z�D}��D~z�D~��Dz�D��D�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD½qD��qD�=qD�}qDýqD��qD�=qD�}qDĽqD��qD�=qD�}qDŽqD��qD�=qD�}qDƽqD��qD�=qD�}qDǽqD��qD�=qD�}qDȽqD��qD�=qD�}qDɽqD��qD�=qD�}qDʽqD��qD�=qD�}qD˽qD��qD�=qD�}qD̽qD��qD�=qD�}qDͽqD��qD�=qD�}qDνqD��qD�=qD�}qDϽqD��qD�=qD�}qDнqD��qD�=qD�}qDѽqD��qD�=qD�}qDҽqD��qD�=qD�}qDӽqD��qD�=qD�}qDԽqD��qD�=qD�}qDսqD��qD�=qD�}qDֽqD��qD�=qD�}qD׽qD��qD�=qD�}qDؽqD��qD�=qD�}qDٽqD��qD�=qD�}qDڽqD��qD�=qD�}qD۽qD��qD�=qD�z>DܽqD��qD�=qD�}qDݽqD��qD�=qD�}qD޽qD��qD�=qD�}qD߽qD��>D�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD��qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��>D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�t�A�r�A�x�A�x�A�r�A�ffA�XA�S�A�"�A���AԸRAԝ�AԅA�dZA�ZA�^5A�`BA�XA�M�A�C�A�$�A�&�A�`BAϲ-A�bA�"�A�
=A�t�A�oA��A�XA��A�x�A��Aȏ\A�ZA���AǓuA��A��A���A��A�x�A���A�K�A�~�A��-A��A�ZA�1A� �A��A���A��A�\)A�1'A�A��A�bNA���A�?}A��hA�"�A��A��A���A�v�A�K�A�33A�VA��\A�+A�-A�5?A�/A��;A��jA���A�
=A��DA�bA�9XA�ZA�C�A�`BA�M�A�n�A�5?A���A�1'A���A���A���A�dZA��!A�v�A�;dA���A�bA�ƨA���A�(�A���A�r�A�A���A�`BA���A���A�{A�z�A��;A��/A�33A�/A��9A��A��jA�XA��9A�ZA�A�A~{A{��Aw�-At�uAs�hAr�\ArM�Ar5?ApbNAm��Al~�AkdZAiO�Ag33AfVAe\)Ad�AdjAd �Act�Ab�DAa�
A`��A_x�A^  A\{A[dZA[VAZ��AX��AWhsAV�`AU��AS&�AQ%AO�FAO
=ANz�AMXAL~�AL�AJ�jAG%AFbAE��AEt�AEoAD�jAD��AD~�ADZADI�ADJAC�^AC\)AB��AB�A@E�A>��A>�uA=��A;A8�`A8�DA8E�A7�
A6�jA5��A4$�A3"�A2r�A1��A1|�A1K�A1�A0�A0�DA/
=A,(�A+/A*I�A(E�A't�A&9XA$Q�A#oA"  A!\)A E�AAl�A�DA$�A�-A��AdZAx�A�A�A5?A+A��A`BAVAr�A�-Ar�A�yAJA��A��AjA|�A�A
M�A	+A��A=qA��A�7A�/AAoA��A�AoA ~�@�o@��7@�%@�j@�|�@�@�%@��/@��D@���@��\@�5?@��7@�j@���@���@�7@�33@��@�^@�t�@�$�@�@㕁@���@�`B@�j@��@ߕ�@�l�@�S�@�@�v�@�@�/@ܴ9@��@��y@��@�p�@�%@أ�@���@�^5@���@��@ҟ�@�$�@�Ĝ@��y@�$�@�@��@�@͑h@̴9@�-@�%@ț�@�b@ǅ@�S�@��@�ȴ@Ɵ�@�n�@�@�x�@���@�+@���@��@� �@�{@�r�@�1@�S�@���@��@��m@�t�@�;d@��y@�J@��@�1@��R@�A�@�x�@��@��m@�ȴ@���@��7@�?}@��@�9X@��@��@�x�@��@�I�@��m@���@�33@���@�^5@��@�7L@��@���@���@�|�@�+@��@���@�V@��#@��#@��#@���@���@�/@��j@�b@���@�S�@�33@�
=@��@���@�-@��@�J@��#@�@��7@�/@��j@�I�@��;@�ƨ@��w@��@�"�@���@���@��\@��+@�v�@�^5@�=q@�@���@�?}@��@��D@��@��@�+@��R@�^5@���@��@�p�@�G�@��@���@�b@��;@���@�;d@�ff@��@��j@�bN@��@���@���@�l�@�S�@�;d@�+@�@��R@�ff@�@�?}@�V@���@��/@���@��9@���@��u@�Q�@�9X@��;@�C�@��@��@��H@�ȴ@��y@�C�@�dZ@���@���@��@�K�@���@��\@��\@��y@��\@��@���@�V@��u@�I�@� �@��@�Z@�j@�A�@���@��@���@�\)@�\)@�t�@���@���@��m@��P@�"�@���@���@���@��+@���@�p�@�O�@��@��@��@��/@�Ĝ@��@�I�@�1@�@K�@~��@~��@~E�@~@}�@}�@}�@}�T@}@}��@}`B@|��@|9X@{t�@z��@y�#@y�7@y%@w�P@wl�@w\)@w+@w�@w�@v��@v�y@v�y@v�@v@u�@t�j@t�D@t(�@t�@s�m@sdZ@r�\@q�^@p��@o��@o�P@o\)@o;d@n�y@n�R@n�R@n��@n�+@nv�@n5?@m@m?}@l��@l�/@l�/@l��@lZ@kdZ@k"�@k"�@k"�@j�H@j�!@j�!@j��@jn�@j^5@i��@ihs@i7L@hĜ@h�u@hr�@hb@g��@g��@gK�@f��@fE�@e�-@e`B@eO�@e/@d�@d��@d��@d9X@d�@d1@c�m@c��@cS�@c33@co@b�!@bn�@bM�@a��@a��@aX@a%@`�`@`Ĝ@`r�@_�w@^ȴ@^ff@]�h@]O�@\z�@[��@[�
@[��@[C�@[o@Z�@Z��@Z�!@Z�!@Z�!@Z~�@ZM�@Y��@Y��@Y7L@X�`@X�u@Xb@W�@W�P@W\)@WK�@W;d@Vȴ@V5?@U�@U�@T�/@T�j@T�@T�D@Tj@T9X@T1@S�@S@R�@R��@R��@R�!@R�\@Rn�@R�@Q�#@Q�7@QX@Q�@P�`@P��@PĜ@P��@P�@PQ�@Pb@O��@O��@O\)@N��@N�+@N{@M@M`B@L��@L��@L9X@K�
@K��@K�@KC�@Ko@J��@I�#@Ix�@IX@I7L@H��@H�9@H�u@H�u@H�u@H�u@HbN@G�@G�@G\)@F��@Fȴ@E��@EV@D�@D�D@Dj@D1@C�F@CdZ@Co@B��@A��@A�^@AX@A%@@r�@@1'@?�;@?�P@?+@>�R@>�R@>��@>��@>�+@>V@>$�@>$�@>E�@>@=�T@=�@=�T@=@=/@<��@<�/@<�j@<�j@<�D@<(�@;�m@;�
@;ƨ@;t�@;@:�@:�H@:��@:�@9�#@9�^@9��@9�7@9�@8�u@8�u@8 �@7�;@7�w@7�@7l�@7
=@6�y@6�R@6��@6�+@6ff@5��@5?}@4�D@4(�@41@3�
@3ƨ@3�F@3��@3�@3dZ@3C�@333@2�H@2��@2��@2M�@1�^@1hs@1X@1%@0��@0bN@/�@/��@/\)@/�@.��@.ff@.{@-@-`B@,�@,I�@+ƨ@+t�@+33@*�H@*n�@)��@)��@)X@)&�@)%@(��@(��@(A�@'l�@';d@'�@'
=@&�@&ff@%�@%p�@%?}@%�@$�/@$�@$z�@$I�@#��@#�F@#�@#S�@#"�@#@"��@"��@"M�@!�^@!X@!7L@!%@ Ĝ@ bN@ bN@ b@��@�w@��@+@�@��@5?@@��@�-@�@�@��@�j@�@z�@9X@(�@(�@�@1@1@��@��@�m@�@@n�@M�@=q@-@J@��@x�@G�@�@��@�`@�`@��@�9@�9@�u@�@bN@1'@�;@�@�@��@�P@�P@|�@K�@
=@�@��@v�@ff@5?@��@�h@�@?}@V@�D@j@I�@9X@�@��@�m@�m@�F@dZ@33@o@@@��@~�@M�@=q@�@��@&�@�@%@��@�9@�u@Q�@b@�;@��@\)@K�@;d@;d@+@��@��@V@5?@{@�-@�h@�@p�@`B@O�@?}@/@��@Z@9X@9X@(�@��@ƨ@��@��@��@��@�@dZ@dZ@dZ@dZ@"�@
�@
��@
��@
�!@
��@
�\@
^5@
=q@
�@	��@	�#@	�#@	�^@	��@	�7@	x�@	hs@	7L@	%@��@�u@Q�@ �@b@  @�;@�;@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�t�A�r�A�x�A�x�A�r�A�ffA�XA�S�A�"�A���AԸRAԝ�AԅA�dZA�ZA�^5A�`BA�XA�M�A�C�A�$�A�&�A�`BAϲ-A�bA�"�A�
=A�t�A�oA��A�XA��A�x�A��Aȏ\A�ZA���AǓuA��A��A���A��A�x�A���A�K�A�~�A��-A��A�ZA�1A� �A��A���A��A�\)A�1'A�A��A�bNA���A�?}A��hA�"�A��A��A���A�v�A�K�A�33A�VA��\A�+A�-A�5?A�/A��;A��jA���A�
=A��DA�bA�9XA�ZA�C�A�`BA�M�A�n�A�5?A���A�1'A���A���A���A�dZA��!A�v�A�;dA���A�bA�ƨA���A�(�A���A�r�A�A���A�`BA���A���A�{A�z�A��;A��/A�33A�/A��9A��A��jA�XA��9A�ZA�A�A~{A{��Aw�-At�uAs�hAr�\ArM�Ar5?ApbNAm��Al~�AkdZAiO�Ag33AfVAe\)Ad�AdjAd �Act�Ab�DAa�
A`��A_x�A^  A\{A[dZA[VAZ��AX��AWhsAV�`AU��AS&�AQ%AO�FAO
=ANz�AMXAL~�AL�AJ�jAG%AFbAE��AEt�AEoAD�jAD��AD~�ADZADI�ADJAC�^AC\)AB��AB�A@E�A>��A>�uA=��A;A8�`A8�DA8E�A7�
A6�jA5��A4$�A3"�A2r�A1��A1|�A1K�A1�A0�A0�DA/
=A,(�A+/A*I�A(E�A't�A&9XA$Q�A#oA"  A!\)A E�AAl�A�DA$�A�-A��AdZAx�A�A�A5?A+A��A`BAVAr�A�-Ar�A�yAJA��A��AjA|�A�A
M�A	+A��A=qA��A�7A�/AAoA��A�AoA ~�@�o@��7@�%@�j@�|�@�@�%@��/@��D@���@��\@�5?@��7@�j@���@���@�7@�33@��@�^@�t�@�$�@�@㕁@���@�`B@�j@��@ߕ�@�l�@�S�@�@�v�@�@�/@ܴ9@��@��y@��@�p�@�%@أ�@���@�^5@���@��@ҟ�@�$�@�Ĝ@��y@�$�@�@��@�@͑h@̴9@�-@�%@ț�@�b@ǅ@�S�@��@�ȴ@Ɵ�@�n�@�@�x�@���@�+@���@��@� �@�{@�r�@�1@�S�@���@��@��m@�t�@�;d@��y@�J@��@�1@��R@�A�@�x�@��@��m@�ȴ@���@��7@�?}@��@�9X@��@��@�x�@��@�I�@��m@���@�33@���@�^5@��@�7L@��@���@���@�|�@�+@��@���@�V@��#@��#@��#@���@���@�/@��j@�b@���@�S�@�33@�
=@��@���@�-@��@�J@��#@�@��7@�/@��j@�I�@��;@�ƨ@��w@��@�"�@���@���@��\@��+@�v�@�^5@�=q@�@���@�?}@��@��D@��@��@�+@��R@�^5@���@��@�p�@�G�@��@���@�b@��;@���@�;d@�ff@��@��j@�bN@��@���@���@�l�@�S�@�;d@�+@�@��R@�ff@�@�?}@�V@���@��/@���@��9@���@��u@�Q�@�9X@��;@�C�@��@��@��H@�ȴ@��y@�C�@�dZ@���@���@��@�K�@���@��\@��\@��y@��\@��@���@�V@��u@�I�@� �@��@�Z@�j@�A�@���@��@���@�\)@�\)@�t�@���@���@��m@��P@�"�@���@���@���@��+@���@�p�@�O�@��@��@��@��/@�Ĝ@��@�I�@�1@�@K�@~��@~��@~E�@~@}�@}�@}�@}�T@}@}��@}`B@|��@|9X@{t�@z��@y�#@y�7@y%@w�P@wl�@w\)@w+@w�@w�@v��@v�y@v�y@v�@v@u�@t�j@t�D@t(�@t�@s�m@sdZ@r�\@q�^@p��@o��@o�P@o\)@o;d@n�y@n�R@n�R@n��@n�+@nv�@n5?@m@m?}@l��@l�/@l�/@l��@lZ@kdZ@k"�@k"�@k"�@j�H@j�!@j�!@j��@jn�@j^5@i��@ihs@i7L@hĜ@h�u@hr�@hb@g��@g��@gK�@f��@fE�@e�-@e`B@eO�@e/@d�@d��@d��@d9X@d�@d1@c�m@c��@cS�@c33@co@b�!@bn�@bM�@a��@a��@aX@a%@`�`@`Ĝ@`r�@_�w@^ȴ@^ff@]�h@]O�@\z�@[��@[�
@[��@[C�@[o@Z�@Z��@Z�!@Z�!@Z�!@Z~�@ZM�@Y��@Y��@Y7L@X�`@X�u@Xb@W�@W�P@W\)@WK�@W;d@Vȴ@V5?@U�@U�@T�/@T�j@T�@T�D@Tj@T9X@T1@S�@S@R�@R��@R��@R�!@R�\@Rn�@R�@Q�#@Q�7@QX@Q�@P�`@P��@PĜ@P��@P�@PQ�@Pb@O��@O��@O\)@N��@N�+@N{@M@M`B@L��@L��@L9X@K�
@K��@K�@KC�@Ko@J��@I�#@Ix�@IX@I7L@H��@H�9@H�u@H�u@H�u@H�u@HbN@G�@G�@G\)@F��@Fȴ@E��@EV@D�@D�D@Dj@D1@C�F@CdZ@Co@B��@A��@A�^@AX@A%@@r�@@1'@?�;@?�P@?+@>�R@>�R@>��@>��@>�+@>V@>$�@>$�@>E�@>@=�T@=�@=�T@=@=/@<��@<�/@<�j@<�j@<�D@<(�@;�m@;�
@;ƨ@;t�@;@:�@:�H@:��@:�@9�#@9�^@9��@9�7@9�@8�u@8�u@8 �@7�;@7�w@7�@7l�@7
=@6�y@6�R@6��@6�+@6ff@5��@5?}@4�D@4(�@41@3�
@3ƨ@3�F@3��@3�@3dZ@3C�@333@2�H@2��@2��@2M�@1�^@1hs@1X@1%@0��@0bN@/�@/��@/\)@/�@.��@.ff@.{@-@-`B@,�@,I�@+ƨ@+t�@+33@*�H@*n�@)��@)��@)X@)&�@)%@(��@(��@(A�@'l�@';d@'�@'
=@&�@&ff@%�@%p�@%?}@%�@$�/@$�@$z�@$I�@#��@#�F@#�@#S�@#"�@#@"��@"��@"M�@!�^@!X@!7L@!%@ Ĝ@ bN@ bN@ b@��@�w@��@+@�@��@5?@@��@�-@�@�@��@�j@�@z�@9X@(�@(�@�@1@1@��@��@�m@�@@n�@M�@=q@-@J@��@x�@G�@�@��@�`@�`@��@�9@�9@�u@�@bN@1'@�;@�@�@��@�P@�P@|�@K�@
=@�@��@v�@ff@5?@��@�h@�@?}@V@�D@j@I�@9X@�@��@�m@�m@�F@dZ@33@o@@@��@~�@M�@=q@�@��@&�@�@%@��@�9@�u@Q�@b@�;@��@\)@K�@;d@;d@+@��@��@V@5?@{@�-@�h@�@p�@`B@O�@?}@/@��@Z@9X@9X@(�@��@ƨ@��@��@��@��@�@dZ@dZ@dZ@dZ@"�@
�@
��@
��@
�!@
��@
�\@
^5@
=q@
�@	��@	�#@	�#@	�^@	��@	�7@	x�@	hs@	7L@	%@��@�u@Q�@ �@b@  @�;@�;@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
�XB
�XB
�XB
�XB
�XB
�XB
�RB
�RB
�RB
�LB
�FB
�3B
�'B
�B
�!B
�-B
�9B
�9B
�3B
�LB
��B:^B��B�B�TB�BJB!�B0!BB�BR�BW
BaHBhsBp�Bu�B�B�7B�PB�uB��B��B��B��B�'BǮB��B��B��B��B��BĜB��B�jB�jB��BɺB��B�-B�!BĜBǮBǮB��B��B�'B��B��B��B��B�DBt�B`BB`BBw�B�=Bw�BiyBhsBt�Bq�Bm�BcTBJ�B8RB$�B�B�BuBPB%B��B�B�)BɺB�dB�B��B��B�1Bw�Bo�B`BBP�BJ�BF�BA�B;dB8RB1'B(�B�B\B%B
�B
�)B
ȴB
ÖB
�jB
�9B
�!B
�B
��B
��B
�7B
n�B
ZB
R�B
M�B
K�B
I�B
>wB
.B
#�B
�B
JB	��B	��B	�B	�B	�B	�sB	�TB	�5B	�B	��B	��B	ŢB	��B	�jB	�XB	�FB	�B	��B	��B	��B	�7B	~�B	w�B	s�B	p�B	jB	ffB	bNB	ZB	I�B	D�B	B�B	A�B	?}B	=qB	<jB	<jB	;dB	:^B	8RB	6FB	33B	0!B	,B	#�B	�B	�B	�B	PB	B	  B��B��B��B��B�B�B�mB�ZB�NB�HB�BB�5B�#B��BȴB��B�^B�?B�RB�?B�!B�B��B��B��B��B��B��B��B��B��B�{B�hB�VB�=B�1B�%B�B�B�B� B}�Bz�Bx�Bw�Bv�Bt�Bs�Br�Bp�Bn�Bm�Bm�Bl�Bk�BjBhsBe`BffBffBffBe`BdZBdZBdZBcTBcTBcTBcTBcTBcTBcTBcTBcTBbNBaHBaHB_;B]/B[#B[#BZBYBZB[#B]/B`BBaHBdZBffBgmBhsBgmBgmBffBe`Be`BgmBgmBgmBjBl�Bl�Bl�Bl�Bl�Bl�Bp�Bp�Bt�Bt�Bw�Bz�B{�B{�B{�B{�B{�Bz�B~�B�B�B�B�B�B�B�B�B�B�B�B�B�1B�JB�JB�PB�oB��B��B��B��B��B��B��B��B��B��B��B��B�B�-B�jB�}B��BŢBȴBɺB��B��B��B�B�B�B�#B�;B�HB�NB�ZB�`B�sB�B�B�B�B�B��B��B	  B	B	B		7B		7B		7B		7B	DB	\B	oB	�B	�B	�B	�B	�B	�B	!�B	%�B	%�B	%�B	'�B	'�B	)�B	,B	/B	1'B	2-B	33B	33B	49B	9XB	@�B	A�B	B�B	B�B	B�B	C�B	D�B	F�B	H�B	K�B	L�B	P�B	T�B	T�B	VB	W
B	YB	]/B	^5B	^5B	_;B	aHB	dZB	gmB	hsB	jB	l�B	t�B	}�B	�B	�%B	�JB	�\B	�hB	�oB	�uB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�'B	�'B	�FB	�^B	�}B	ĜB	ȴB	ɺB	ȴB	ȴB	ǮB	ƨB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�#B	�)B	�/B	�;B	�NB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
+B
1B
	7B
	7B
	7B
	7B
	7B
	7B

=B
DB
JB
JB
JB
JB
PB
VB
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
oB
oB
uB
uB
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
#�B
#�B
#�B
#�B
$�B
$�B
%�B
%�B
&�B
&�B
'�B
'�B
'�B
'�B
(�B
(�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
+B
,B
,B
,B
,B
,B
,B
,B
,B
,B
-B
-B
-B
-B
.B
.B
.B
.B
.B
.B
/B
/B
/B
/B
0!B
0!B
0!B
1'B
1'B
1'B
2-B
2-B
2-B
2-B
33B
33B
33B
49B
5?B
5?B
5?B
5?B
6FB
6FB
6FB
6FB
5?B
5?B
6FB
6FB
7LB
7LB
7LB
9XB
:^B
:^B
:^B
:^B
;dB
;dB
;dB
;dB
<jB
<jB
=qB
=qB
=qB
=qB
>wB
>wB
>wB
?}B
?}B
?}B
?}B
?}B
>wB
>wB
>wB
>wB
>wB
>wB
>wB
>wB
>wB
>wB
?}B
?}B
?}B
?}B
?}B
>wB
?}B
?}B
@�B
?}B
@�B
A�B
B�B
A�B
B�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
E�B
F�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
G�B
H�B
H�B
I�B
J�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
M�B
M�B
M�B
M�B
M�B
N�B
N�B
O�B
O�B
O�B
O�B
P�B
P�B
P�B
Q�B
Q�B
R�B
R�B
R�B
S�B
S�B
T�B
T�B
T�B
T�B
T�B
T�B
VB
VB
W
B
XB
XB
XB
XB
XB
XB
YB
YB
YB
ZB
ZB
ZB
ZB
ZB
[#B
[#B
[#B
[#B
[#B
[#B
[#B
\)B
\)B
]/B
]/B
]/B
]/B
^5B
^5B
^5B
^5B
^5B
^5B
_;B
_;B
_;B
_;B
`BB
`BB
`BB
aHB
aHB
aHB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
cTB
cTB
dZB
dZB
dZB
e`B
dZB
e`B
e`B
e`B
ffB
ffB
ffB
ffB
ffB
ffB
ffB
ffB
ffB
ffB
gmB
gmB
gmB
gmB
gmB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
iyB
iyB
iyB
iyB
jB
jB
jB
jB
jB
jB
jB
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
l�B
m�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
w�B
x�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
z�B
z�B
z�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B
�eB
�PB
�ZB
�vB
��B
��B
�zB
�/B
��B
��B
��B
��B
��B
�GB
�B
�/B
�^B
�aB
�\B
��B
ôB=�B��BהB�B��B9B#B0�BD�BS�BY�Bc|BjBq�BwaB��B�1B�.B�mB��B��B��B�yB��B�3BβB��B�}B�BԳB��BäB�.B�AB�VB�~B�vB�YB��B�(B� BˣB�%BĘB�
B��B�&B�@B��B��B}BdQB`pByIB�XB|�Bp�Bj�BwBuBq�Bq,BSlBBB)$B�B�B�B}B\B�$B�MB�*B�TB��B�RB�CB��B��BzqBw<BgBS=BK�BH�BC�B</B:�B3�B+tB!�B%B�B
��B
�B
�FB
�8B
�5B
��B
��B
��B
��B
�"B
��B
u�B
\^B
UUB
NwB
L@B
N?B
D�B
1kB
&�B
 �B
�B	�)B	�FB	�B	�CB	�ZB	�<B	�B	�B	�]B	� B	�|B	�DB	�GB	�BB	�|B	�-B	��B	�$B	��B	��B	�9B	�1B	y^B	uB	sYB	l�B	g�B	e�B	b�B	L2B	E^B	C|B	B�B	@`B	=�B	<�B	<�B	;�B	; B	9B	7B	4B	1B	0sB	&�B	 XB	tB	"B	�B	�B	 �B	 B��B��B�2B�,B�3B��B�;B��B��B�B�RB��BؑB�BýB�=B�SB�iB��B�B��B��B�iB��B�uB��B��B��B��B�#B�)B�|B��B�B��B��B�4B��B��B��B�#B~�B{Bx�Bx�Bv[Bv>Bs�Br�Bq�Bo;Bn�Bm�BlpBlsBk�Bk�BgvBg�BhTBf�Bf�BfBd�BdBdnBe'Bd�Bc�Bc�BdBd�Bc�Bc;Bb�BcKBa�BaB]�B[�B[�B\B[�B]=B^kBaQBcBe�BgBg�Bh�Bg�Bg�Bg&BfQBf/BhBhFBiBk�BmABm(Bm Bm�Bn�Bn�Bq�Br[Bu�Bv�Bz6B{�B| B|B|&B|AB}DB~uB��B��B��B��B�dB�zB��B�`B�uB��B��B�OB��B�xB�>B��B��B��B�:B��B�(B�-B��B��B�5B�kB�1B�mB��B�5B��B�#B��B�sB�?B��B�kB�3B�[B� B��B��B�:B��B�&B��B�B��B�B�^B�B�B��B��B�!B��B�HB�AB	 gB	hB	�B		BB		BB		MB		uB	�B	�B	hB	<B	�B	�B	�B	B	!B	"jB	&B	&
B	&9B	(%B	(UB	*�B	,�B	/�B	1�B	2]B	3JB	3gB	51B	:*B	@�B	A�B	B�B	B�B	B�B	C�B	EB	GB	I�B	LRB	MyB	Q�B	U�B	U�B	V�B	W�B	Z"B	]yB	^XB	^B	_�B	bB	e(B	g�B	h�B	k"B	m�B	v	B	B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�}B	��B	��B	��B	��B	�B	��B	�	B	�JB	�.B	�zB	��B	�XB	�ZB	�EB	�LB	�'B	��B	�\B	�gB	ȽB	��B	�B	�yB	��B	ƭB	�FB	�RB	�qB	�GB	��B	�qB	�4B	�B	��B	њB	��B	�KB	؈B	ًB	�JB	�{B	�6B	�B	�B	��B	�B	�B	�)B	��B	�B	�nB	��B	�B	�B	�B	��B	��B	�B	�B	��B	�B	�B	�B	�B	�B	��B	�B	�B	�B	��B	��B	��B	��B	��B	��B	�B	�7B	��B	�vB	�]B	��B	�B	�FB	��B	�B	�	B	�B	�B	�B	�B	�B	�B	�B	��B	��B
MB
6B
SB
 B
7B
mB
�B
�B
�B
�B
WB
PB
DB
gB
XB
	AB
	KB
	XB
	MB
	qB
	�B

�B
zB
kB
SB
bB
�B
�B
�B
hB
hB
�B
�B
nB
yB
�B
~B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
KB
jB
B
 dB
!B
 bB
!/B
!�B
"B
"B
"�B
"�B
"�B
"�B
"�B
"�B
#B
$B
$B
$$B
$5B
%+B
%'B
&NB
&;B
'	B
'B
(B
(B
(ZB
(mB
)�B
)DB
*.B
*B
*B
*B
*B
*(B
*&B
*XB
+ZB
,B
,&B
,B
,*B
,(B
,+B
,JB
,@B
,KB
-;B
-AB
-7B
-!B
.*B
.4B
.4B
.>B
.KB
.KB
/HB
/VB
/hB
/xB
0�B
0eB
0pB
1vB
1vB
1|B
2|B
2XB
2SB
2iB
3cB
3|B
3�B
4�B
5_B
5dB
5xB
5zB
6iB
6OB
6PB
6PB
5qB
5�B
6�B
6�B
7�B
7�B
8B
9�B
:�B
:�B
:�B
:�B
;�B
;�B
;�B
;�B
<�B
<�B
=�B
=�B
=�B
=�B
>�B
>�B
>�B
?�B
?�B
?�B
?�B
?�B
>�B
>�B
>�B
>kB
>�B
>�B
>xB
>�B
>�B
>�B
?�B
?�B
?�B
?�B
?�B
>�B
?�B
?�B
@�B
?�B
@�B
A�B
B�B
A�B
CB
C�B
C�B
C�B
C�B
D B
EB
D�B
FB
F�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
H6B
I7B
IOB
JB
J�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
LB
K�B
K�B
LB
MRB
MB
M�B
N!B
NB
NBB
N8B
O&B
OB
P"B
PBB
P!B
P*B
Q1B
Q<B
QFB
RfB
RHB
S.B
S$B
S+B
TIB
T`B
U%B
U.B
U)B
UB
U*B
U(B
VOB
V�B
W2B
X/B
X%B
X=B
XcB
XeB
XqB
YAB
Y6B
YNB
ZGB
ZGB
ZLB
ZbB
ZWB
[OB
[QB
[SB
[EB
[NB
[TB
[lB
\�B
\zB
]TB
]_B
]kB
]�B
^BB
^�B
^pB
^LB
^[B
^�B
_�B
_}B
_�B
_nB
`vB
`kB
`�B
axB
akB
abB
bjB
b�B
b�B
bfB
b[B
biB
bfB
bYB
beB
bZB
blB
b�B
c�B
c�B
dB
dpB
dsB
e�B
d�B
e�B
e�B
e�B
f�B
f�B
ftB
f�B
f�B
ftB
f�B
f�B
f�B
f�B
g�B
g�B
gwB
g�B
g�B
h}B
h�B
h�B
h�B
h�B
h�B
h�B
i�B
i�B
i�B
i�B
j�B
j�B
j�B
kB
j�B
j�B
j�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
m
B
nB
n�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
qB
q�B
q�B
q�B
rB
r�B
r�B
r�B
r�B
r�B
r�B
r�B
sB
t'B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
w�B
yB
zB
zB
zB
zB
y�B
y�B
y�B
z�B
z�B
z�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<$��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<*�9<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<l)�<#�
</mT<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<%��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - (REPORTED_SURFACE_PRESSURE) for PADJ                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al,2007,JAOT & effects of pressure adjustments                                                                                                                                                            PADJ REPORTED_SURFACE_PRESSURE =0.08 dbar                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CTM alpha = 0.141 & tau = 6.68 s with error equal to the correction                                                                                                                                                                                             Pressures adjusted using reported surface pressure when warranted.                                                                                                                                                                                              none                                                                                                                                                                                                                                                            Salinity corrected for pressure sensor calibration drift and cell thermal mass                                                                                                                                                                                  201810310904292018103109042920181031090429  AO  ARCAADJP                                                                    20170601170224    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20170601170224  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20170601170224  QCF$                G�O�G�O�G�O�0               PM  ARSQPADJV1.1                                                                20181031090429  QC  PRES            @���D��fG�O�                PM  ARSQCTM V1.1                                                                20181031090429  QC  PSAL            @���D��fG�O�                PM  ARSQOWGUV1.0                                                                20181103100347  IP                  G�O�G�O�G�O�                