CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:44:09Z creation;2022-06-04T17:44:10Z conversion to V3.1      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  78   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7x   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8<   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8@   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8H   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8L   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8T   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8\   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8d   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8h   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8p   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8t   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8x   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8|   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9|   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I8   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M(   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tx   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �0   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �p   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �(   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ޘ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �    HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �D   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �T   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �X   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �h   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �l   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �p   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �tArgo profile    3.1 1.2 19500101000000  20220604174409  20220610141505  5905853                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               wA   JA                                  2B  A   APEX                            8421                            2.11.2                          846 @��X�=ѻ1   @��Y[�ޠ@0KI�^�c{-1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @33@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�ffB�ffB���B�  B���B�  B�ffB�33B�  B�  B���B�  B���B���B���B�  B�  B�  B�ffB�  B�  B���B���C�fC  C  C  C
  C  C  C  C  C  C  C  C  C  C  C � C!� C$  C&  C(  C*  C,�C.�C0  C2  C3�fC6  C8  C:  C;�fC>  C?�fCB  CD  CF  CH  CJ�CL33CN  CP  CQ�fCS�fCV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$�fD%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DFfDF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D�fD�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�ff1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @{@z�H@�p�@�p�A�RA>�RA^�RA~�RA�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)B�B�B�B�B'�B/�B7�B?�BG�BO�BW�B_�Bg�Bo�Bw�B�B��
B��
B��
B��
B��
B��
B��
B��
B�
=B�=pB�=pB���B��
B���B��
B�=pB�
=B��
B��
Bϣ�B��
Bף�Bۣ�Bߣ�B��
B��
B��
B�=pB��
B��
B���B���C��C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C k�C!k�C#�C%�C'�C)�C,C.C/�C1�C3��C5�C7�C9�C;��C=�C?��CA�CC�CE�CG�CJCL�CM�CO�CQ��CS��CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D z�D ��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��D	z�D	��D
z�D
��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dt{D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��D z�D ��D!z�D!��D"z�D"��D#z�D#��D$�GD$��D%z�D%��D&z�D&��D'z�D'��D(z�D(��D)z�D)��D*z�D*��D+z�D+��D,z�D,��D-z�D-��D.z�D.��D/z�D/��D0z�D0��D1z�D1��D2z�D2��D3z�D3��D4z�D4��D5z�D5��D6z�D6��D7z�D7��D8z�D8��D9z�D9��D:z�D:��D;z�D;��D<z�D<��D=z�D=��D>z�D>��D?z�D?��D@z�D@��DAz�DA��DBz�DB��DCz�DC��DDz�DD��DEz�DFGDFz�DF��DGz�DG��DHz�DH��DIz�DI��DJz�DJ��DKz�DK��DLz�DL��DMz�DM��DNz�DN��DOz�DO��DPz�DP��DQz�DQ��DRz�DR��DSz�DS��DTz�DT��DUz�DU��DVz�DV��DWz�DW��DXz�DX��DYz�DY��DZz�DZ��D[z�D[��D\z�D\��D]z�D]��D^z�D^��D_z�D_��D`z�D`��Daz�Da��Dbz�Db��Dcz�Dc��Ddz�Dd��Dez�De��Dfz�Df��Dgz�Dg��Dhz�Dh��Diz�Di��Djz�Dj��Dkz�Dk��Dlz�Dl��Dmz�Dm��Dnz�Dn��Doz�Do��Dpz�Dp��Dqz�Dq��Drz�Dr��Dsz�Ds��Dtz�Dt��Duz�Du��Dvz�Dv��Dwz�Dw��Dxz�Dx��Dyz�Dy��Dzz�Dz��D{z�D{��D|z�D|��D}z�D}��D~z�D~��D�GD��D�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD½qD��qD�=qD�}qDýqD��qD�=qD�}qDĽqD��qD�=qD�}qDŽqD��qD�=qD�}qDƽqD��qD�=qD�}qDǽqD��qD�=qD�}qDȽqD��qD�=qD�}qDɽqD��qD�=qD�}qDʽqD��qD�=qD�}qD˽qD��qD�=qD�}qD̽qD��qD�=qD�}qDͽqD��qD�=qD�}qDνqD��qD�=qD�}qDϽqD��qD�=qD�}qDнqD��qD�=qD�}qDѽqD��qD�=qD�}qDҽqD��qD�=qD�}qDӽqD��qD�=qD�}qDԽqD��qD�=qD�}qDսqD��qD�=qD�}qDֽqD��qD�=qD�}qD׽qD��qD�=qD�}qDؽqD��qD�=qD�}qDٽqD��qD�=qD�}qDڽqD��qD�=qD�}qD۽qD��qD�=qD�}qDܽqD��qD�=qD�}qDݽqD��qD�=qD�}qD޽qD��qD�=qD�}qD߽qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD��qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�c�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A̋�A̔{A̒:A̋A̋xǍ�A̘_A̞�A̡�Ạ:ẠẠnA̤@A̤�ḀzĄXĄXḀzA̦LA̦�Ą�Ą�A̫6A̬A̮A̮}A̯OA̮�A̯OA̮A̮}A̭A̮}A̮A̬=Ḁ�Ą�A̢�A̩�A�v�A��]A�c A���A�YA�ÖA�g�A�ҽA��$A�OBA�{�A�|�A�^A��A���A���A��2A�1�A�(�A���A�@�A��5A��)A�;0A�~]A��VA���A�v�A��A�=qA��A�S�A���A���A�H�A�kA�W?A��A���A�_A~��A|��At�UApѷAn��Ak��Ai	lAf��Ad(�A`�AZ�FAVA�APѷAM�AK2aAHAF_AB�A@�A?1A=A;�aA:��A:=qA9>�A7�7A7qA7!�A7��A7�A7�A6u%A5GEA4�9A4�XA2�]A2JA1K�A0��A0	A/
�A.?}A-�A+�AA+4A+�A*��A*��A*u�A)~(A(�2A(��A(OA'��A'&�A&�A&�jA&یA&��A&YA$�A#�RA"��A"��A"HA"	�A!�1A �2A ~�A XyA��A�MAA�An/AW?A�uAE�A�AU�A�]A�cA�AX�A.�AA�]A�A��AxlA?Ac�A=A�!A��A0UAe�A`BA��A+kA�A�'AیA?AخA��Ar�A �A�KAFA�A�OAb�A�>Al�AZA~A
˒A
�<A
��A
  A	w�A	1�A	4A	7A�nAf�A�?Av�AYKA�A{JAD�A�&A��AA�A�Av�AA�mA�OAC-AqA �A}�A�A=A �SA (�@��@��@��@���@���@�5?@��k@��@���@�)�@��@��z@�c@�hs@�
=@��.@�N�@��@��@�IR@��,@��[@���@�@�@��/@�@��@��@�@�4@��|@�}@��]@��@@��@췀@�@O@��&@�y�@��@�j@�@@�	l@� \@�4@�Vm@�V@��@�3�@��@�A�@�V@䗍@�@��@�[@�<�@�~@�K�@��,@�K�@�N<@��@�l"@�?�@�u@ݷ�@�;d@�L0@ۜ�@�x@�a@�o@���@��	@�l"@�&�@��@ץ�@�x�@�ȴ@��@�J�@�!@��@�=q@�c@�S�@�K�@�Y@Ч@��A@��"@�!@�+�@�ѷ@�[�@��;@�b�@���@�r�@��@��@ɓ@�!-@Ȏ�@��@�P�@Ƙ_@ƔF@ů�@�K�@��2@�H@��@�� @�@��)@¥z@�'R@�u@�4@�V@��@��"@�1'@���@�͟@��@��m@��'@�x@�x@���@�9X@��>@���@�a�@��@�H�@�|�@�\�@��@���@��z@���@���@�V@��o@���@��{@�2a@�"�@���@��s@�Ta@��@���@�j�@�7L@���@�GE@���@��@���@�O�@�1�@��@��x@�a|@�_�@�W�@�RT@��@��z@��A@�4n@��@�ں@��B@�Ĝ@��@�	�@���@�rG@��`@�Ĝ@�s�@�~@��-@�X@��@��H@��b@�@�@�خ@�y�@��f@�}V@�8�@�|@���@�YK@��@��h@���@���@�N<@���@��@�S&@��@�($@��`@��#@�C�@�<�@��@��N@���@���@�:�@��j@���@�Ɇ@�@�(�@�Q�@�W?@�K�@�:�@�C�@�^�@�o @�Dg@�+�@�@���@��`@��'@�tT@�$�@��@���@�W?@�&@��@���@��X@���@�M@��Z@�ԕ@���@��{@�%F@��K@��p@�v�@���@��:@�a�@�L�@�C�@�:�@�(�@�@���@�}V@�#:@��r@��o@��@��@�c�@�U�@�;d@��E@��@��@��}@��@�J#@��f@���@��6@�u�@�[�@�,=@��@��N@��@�1�@��@��)@��o@�@���@�g�@�=@��@���@��/@��Y@�Ft@�.�@���@��@���@�X@�9�@�C@��@���@�֡@�H@���@���@�[W@�2a@�v�@��Z@���@��P@�a�@�F�@�V@���@��<@���@�J�@�<�@�@��M@�'�@���@�>B@�5?@�/�@�)�@�u@���@�=�@��@��v@��@�Ĝ@�0U@���@��@�C@���@�Z�@�Ta@�L0@�*�@�M@���@��@��T@��@��h@�hs@�\)@��@��o@�?@�6�@�_@�F@��@�k@�	@iD@6z@)_@@~�8@~�<@~��@~q�@~($@}��@|�@|-�@|1@{�@{��@{Mj@z�h@z��@z�@zOv@y��@ye,@y*0@x�@x�@w�[@w��@wW?@w�@v��@v$�@u�@u�S@uY�@u*0@tی@t~(@s��@s>�@s�@rߤ@rh
@q�@q��@q��@q��@q2a@q@q@p�@o�6@oRT@n�8@nl�@nJ�@n+k@m�9@mhs@l��@l��@lq@lx@k+@j��@jOv@j.�@je@i�Z@iJ�@iDg@i:�@iV@h��@g�;@g��@g$t@f�"@f@eJ�@e2a@eIR@e&�@e%@dĜ@d]d@d?�@c�a@c{J@b��@bu%@b&�@b �@a�j@a��@`��@`|�@`x@_Z�@^�R@]��@]@\�@\[�@\�@[ƨ@[!-@Zں@ZB[@Y��@Y��@Y�@X��@X4n@W�]@Wݘ@W��@W��@W��@WRT@V��@V_�@U��@U-w@T�K@T�.@T`�@T�@S��@S@O@SC@R�L@R0U@Q�T@Q��@Q��@P�f@P�@O�@O��@Ot�@OZ�@OA�@O)_@N�"@N��@N($@N{@N�@M�D@M��@M�@Mw2@ML�@M<6@M#�@L�@K�r@K~�@K>�@J��@J@�@JJ@I�o@I��@IN<@IV@H��@Hy>@H�@G��@Gqv@GJ#@F�]@F&�@E��@Ep�@EQ�@E�@D�K@D|�@D`�@D[�@DI�@D@C�K@C�V@C+@B�@B�H@B�H@B��@B5?@A�@A8�@@�p@@�@@H@?�m@?�@>xl@>5?@>�@=�@=c�@=-w@<�@<�_@<w�@<h�@<'R@;iD@;A�@;"�@:�!@9�@9w2@9L�@92a@9#�@9+@9�@8�	@8�@8�u@7�@7�@6��@6d�@6GE@61�@5�@5�X@5f�@5&�@4ی@4��@4��@4N�@3�]@3H�@2��@2��@2n�@2^5@2=q@1�@1��@1ϫ@1O�@1�@0ی@0�@0:�@0"h@0�@/�*@/.I@.�@.i�@-��@,�P@,�e@,A�@,�@+�Q@+��@+C�@+=@+6z@+C@*ں@*6�@)�Z@)��@)�n@(�@(��@(�j@(�@(r�@(�@'�&@'�6@'��@';d@&�@&z@&($@%�j@%��@%�n@%��@%[W@$��@$��@$`�@$�@$�@#��@#��@#�@#��@#�@#��@#{J@#RT@#&@#@#
=@"�M@"��@"�c@"�c@"ߤ@"z@"1�@!�D@!�#@!��@!�S@!w2@!A @!!�@ �p@ �u@ r�@�@K�@�y@��@��@xl@l�@R�@#:@��@Y�@0�@	l@��@�@w�@b@ݘ@��@��@v`@S@�F@Q@��@�X@L�@/@�@�@��@u�@l"@j@V�@%�@G@�A@�}@dZ@�@
=@��@�x@��@?@�@�@�9@��@A @!�@�5@tT@�@��@6z@��@��@�@ں@��@n�@B[@&�@4@��@�@��@�S@s�@G�@2a@%F@@@��@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A̋�A̔{A̒:A̋A̋xǍ�A̘_A̞�A̡�Ạ:ẠẠnA̤@A̤�ḀzĄXĄXḀzA̦LA̦�Ą�Ą�A̫6A̬A̮A̮}A̯OA̮�A̯OA̮A̮}A̭A̮}A̮A̬=Ḁ�Ą�A̢�A̩�A�v�A��]A�c A���A�YA�ÖA�g�A�ҽA��$A�OBA�{�A�|�A�^A��A���A���A��2A�1�A�(�A���A�@�A��5A��)A�;0A�~]A��VA���A�v�A��A�=qA��A�S�A���A���A�H�A�kA�W?A��A���A�_A~��A|��At�UApѷAn��Ak��Ai	lAf��Ad(�A`�AZ�FAVA�APѷAM�AK2aAHAF_AB�A@�A?1A=A;�aA:��A:=qA9>�A7�7A7qA7!�A7��A7�A7�A6u%A5GEA4�9A4�XA2�]A2JA1K�A0��A0	A/
�A.?}A-�A+�AA+4A+�A*��A*��A*u�A)~(A(�2A(��A(OA'��A'&�A&�A&�jA&یA&��A&YA$�A#�RA"��A"��A"HA"	�A!�1A �2A ~�A XyA��A�MAA�An/AW?A�uAE�A�AU�A�]A�cA�AX�A.�AA�]A�A��AxlA?Ac�A=A�!A��A0UAe�A`BA��A+kA�A�'AیA?AخA��Ar�A �A�KAFA�A�OAb�A�>Al�AZA~A
˒A
�<A
��A
  A	w�A	1�A	4A	7A�nAf�A�?Av�AYKA�A{JAD�A�&A��AA�A�Av�AA�mA�OAC-AqA �A}�A�A=A �SA (�@��@��@��@���@���@�5?@��k@��@���@�)�@��@��z@�c@�hs@�
=@��.@�N�@��@��@�IR@��,@��[@���@�@�@��/@�@��@��@�@�4@��|@�}@��]@��@@��@췀@�@O@��&@�y�@��@�j@�@@�	l@� \@�4@�Vm@�V@��@�3�@��@�A�@�V@䗍@�@��@�[@�<�@�~@�K�@��,@�K�@�N<@��@�l"@�?�@�u@ݷ�@�;d@�L0@ۜ�@�x@�a@�o@���@��	@�l"@�&�@��@ץ�@�x�@�ȴ@��@�J�@�!@��@�=q@�c@�S�@�K�@�Y@Ч@��A@��"@�!@�+�@�ѷ@�[�@��;@�b�@���@�r�@��@��@ɓ@�!-@Ȏ�@��@�P�@Ƙ_@ƔF@ů�@�K�@��2@�H@��@�� @�@��)@¥z@�'R@�u@�4@�V@��@��"@�1'@���@�͟@��@��m@��'@�x@�x@���@�9X@��>@���@�a�@��@�H�@�|�@�\�@��@���@��z@���@���@�V@��o@���@��{@�2a@�"�@���@��s@�Ta@��@���@�j�@�7L@���@�GE@���@��@���@�O�@�1�@��@��x@�a|@�_�@�W�@�RT@��@��z@��A@�4n@��@�ں@��B@�Ĝ@��@�	�@���@�rG@��`@�Ĝ@�s�@�~@��-@�X@��@��H@��b@�@�@�خ@�y�@��f@�}V@�8�@�|@���@�YK@��@��h@���@���@�N<@���@��@�S&@��@�($@��`@��#@�C�@�<�@��@��N@���@���@�:�@��j@���@�Ɇ@�@�(�@�Q�@�W?@�K�@�:�@�C�@�^�@�o @�Dg@�+�@�@���@��`@��'@�tT@�$�@��@���@�W?@�&@��@���@��X@���@�M@��Z@�ԕ@���@��{@�%F@��K@��p@�v�@���@��:@�a�@�L�@�C�@�:�@�(�@�@���@�}V@�#:@��r@��o@��@��@�c�@�U�@�;d@��E@��@��@��}@��@�J#@��f@���@��6@�u�@�[�@�,=@��@��N@��@�1�@��@��)@��o@�@���@�g�@�=@��@���@��/@��Y@�Ft@�.�@���@��@���@�X@�9�@�C@��@���@�֡@�H@���@���@�[W@�2a@�v�@��Z@���@��P@�a�@�F�@�V@���@��<@���@�J�@�<�@�@��M@�'�@���@�>B@�5?@�/�@�)�@�u@���@�=�@��@��v@��@�Ĝ@�0U@���@��@�C@���@�Z�@�Ta@�L0@�*�@�M@���@��@��T@��@��h@�hs@�\)@��@��o@�?@�6�@�_@�F@��@�k@�	@iD@6z@)_@@~�8@~�<@~��@~q�@~($@}��@|�@|-�@|1@{�@{��@{Mj@z�h@z��@z�@zOv@y��@ye,@y*0@x�@x�@w�[@w��@wW?@w�@v��@v$�@u�@u�S@uY�@u*0@tی@t~(@s��@s>�@s�@rߤ@rh
@q�@q��@q��@q��@q2a@q@q@p�@o�6@oRT@n�8@nl�@nJ�@n+k@m�9@mhs@l��@l��@lq@lx@k+@j��@jOv@j.�@je@i�Z@iJ�@iDg@i:�@iV@h��@g�;@g��@g$t@f�"@f@eJ�@e2a@eIR@e&�@e%@dĜ@d]d@d?�@c�a@c{J@b��@bu%@b&�@b �@a�j@a��@`��@`|�@`x@_Z�@^�R@]��@]@\�@\[�@\�@[ƨ@[!-@Zں@ZB[@Y��@Y��@Y�@X��@X4n@W�]@Wݘ@W��@W��@W��@WRT@V��@V_�@U��@U-w@T�K@T�.@T`�@T�@S��@S@O@SC@R�L@R0U@Q�T@Q��@Q��@P�f@P�@O�@O��@Ot�@OZ�@OA�@O)_@N�"@N��@N($@N{@N�@M�D@M��@M�@Mw2@ML�@M<6@M#�@L�@K�r@K~�@K>�@J��@J@�@JJ@I�o@I��@IN<@IV@H��@Hy>@H�@G��@Gqv@GJ#@F�]@F&�@E��@Ep�@EQ�@E�@D�K@D|�@D`�@D[�@DI�@D@C�K@C�V@C+@B�@B�H@B�H@B��@B5?@A�@A8�@@�p@@�@@H@?�m@?�@>xl@>5?@>�@=�@=c�@=-w@<�@<�_@<w�@<h�@<'R@;iD@;A�@;"�@:�!@9�@9w2@9L�@92a@9#�@9+@9�@8�	@8�@8�u@7�@7�@6��@6d�@6GE@61�@5�@5�X@5f�@5&�@4ی@4��@4��@4N�@3�]@3H�@2��@2��@2n�@2^5@2=q@1�@1��@1ϫ@1O�@1�@0ی@0�@0:�@0"h@0�@/�*@/.I@.�@.i�@-��@,�P@,�e@,A�@,�@+�Q@+��@+C�@+=@+6z@+C@*ں@*6�@)�Z@)��@)�n@(�@(��@(�j@(�@(r�@(�@'�&@'�6@'��@';d@&�@&z@&($@%�j@%��@%�n@%��@%[W@$��@$��@$`�@$�@$�@#��@#��@#�@#��@#�@#��@#{J@#RT@#&@#@#
=@"�M@"��@"�c@"�c@"ߤ@"z@"1�@!�D@!�#@!��@!�S@!w2@!A @!!�@ �p@ �u@ r�@�@K�@�y@��@��@xl@l�@R�@#:@��@Y�@0�@	l@��@�@w�@b@ݘ@��@��@v`@S@�F@Q@��@�X@L�@/@�@�@��@u�@l"@j@V�@%�@G@�A@�}@dZ@�@
=@��@�x@��@?@�@�@�9@��@A @!�@�5@tT@�@��@6z@��@��@�@ں@��@n�@B[@&�@4@��@�@��@�S@s�@G�@2a@%F@@@��@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�;B��B�!B�UB�UB�UB�!B�B�!B�!B�!B�;B�B�B�!B�;B�;B�!B�!B�!B�!B�B�;B�!B�;B�UB�UB�UB�;B�B��B��B�B�!B�oB��B�oB�B�UB��B	�B	,�B	�B
y�B
�tB��B�|B��B�iB�B��B��B��B�;B�B��B��B��B�JB��B��BtTBX�B7�B"�B�B
�B
�B
ؓB
��B
��B
�gB
~�B
f2B
ZQB
PB
?cB
+B
�B	�B	��B	�	B	��B	��B	��B	zxB	m�B	_VB	KxB	33B	�B	B	�B	�B��B�B��B�^B	 �B	 �B��B�B��B��B	EB	�B	'�B	>B	G�B	K�B	GEB	J=B	O�B	\�B	a�B	i�B	y�B	�oB	�YB	��B	�B	��B	��B	�:B	��B	�B	�#B	��B	��B	�/B	��B	��B	��B	�B	�B	�wB	�B	�iB	�aB	�B	�@B	�B	��B
B
�B

�B
B
B
B
�B
�B
�B
)B
JB
�B
	7B
�B	�6B	�XB	�+B	��B	�%B	�|B	�-B	��B	��B
3B
GB	�B	�*B	�fB	��B	�/B	��B	�jB
 �B
�B
�B
�B
�B
�B
,B
B
	B
�B
	�B
zB
?B
 B	�JB	�B	�nB	�hB	�B	��B	�OB	��B	�B	�wB	�}B	��B	�GB	�B	��B	�6B	�VB	��B
  B
 iB
 B
'B
�B
�B
B
�B
�B
aB
�B
B
�B
�B
MB
B
MB
uB
�B
 B
 4B
 �B
 �B
B
�B

rB
	�B

	B

#B
	�B

	B
	�B
	�B
	�B
	RB
	B
�B
1B
�B
�B
�B
�B
EB
_B
B
EB
_B
EB
+B
�B
EB
�B
�B
tB
YB
�B
{B
;B	�wB	�0B	�	B	�B	��B	�PB	�xB	�$B	��B	�B	��B	��B	�}B	�B	��B	�wB	��B	��B	��B	��B	��B	�aB	��B	�B	�B	�B	�XB	��B	�dB	�B	�B	��B	�B	�JB	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�qB	�qB	�"B	��B	�B	�>B	�DB	��B	�xB	��B	��B	�]B	��B	�6B	��B	��B	��B	�	B	��B	��B	�	B	�>B	�rB	�rB	�XB	��B	�lB	�8B	�B	��B	�LB	�B	�zB	��B	�%B	�%B	�LB	�+B	��B	�B	�B	�3B	�B	�FB	��B	�+B	�B	��B	�aB	�-B	��B	�B	�B	�MB	�B	�B	�B	�B	��B	�B	��B	�zB	��B	�fB	�fB	�2B	�B	��B	�B	�2B	��B	��B	��B	��B	�	B	�B	�^B	��B	��B	��B	�>B	�rB	�B	�B	�xB	�DB	��B	��B	��B	��B	��B	�jB	��B	��B	��B	��B
B
 �B	�}B	�cB	�cB	�.B	�cB
 B
 4B
 �B
 B
;B
�B
�B
[B
�B
�B
�B
�B
B
GB
aB
�B
�B
B
B
�B
�B
[B
{B
�B
�B
�B
[B
oB
 4B	�}B	��B	�rB	�+B	��B	��B	�B	��B	�GB	��B	�'B	�B	�GB	�B	�^B	�B	�BB	�cB
 �B
UB
�B
�B
	7B
	�B
	�B

=B
xB
�B
JB
�B
�B
<B
�B
�B
�B
�B
�B
 B
�B
[B
FB
{B
�B
2B
SB
SB
mB

B
_B
�B
B
1B
1B
1B
1B
1B
1B
�B
kB
�B
�B
#B
=B
=B
#B
WB
�B
)B
]B
�B
�B
/B
IB
/B
/B
/B
/B
B
/B
dB
�B
jB
B
!B
�B
 �B
!B
!|B
!�B
!�B
!�B
"B
"�B
"�B
"�B
# B
# B
#nB
#�B
#�B
#�B
#�B
#�B
#�B
$�B
$�B
%�B
%�B
%�B
&�B
&�B
&�B
'8B
'�B
'�B
($B
($B
($B
'�B
'�B
'8B
(�B
(�B
)yB
*0B
)yB
)_B
)DB
(�B
(�B
(�B
)yB
)�B
)�B
)yB
)yB
)�B
)yB
)�B
*B
*�B
+B
*�B
*�B
+QB
+�B
+�B
+�B
+�B
+�B
+�B
,B
,"B
-)B
,�B
-)B
-CB
-]B
-]B
-wB
-wB
-wB
-�B
.IB
.IB
.�B
/OB
0�B
0�B
1�B
1vB
1�B
2B
2|B
2�B
3B
4TB
5B
5ZB
5ZB
5ZB
5tB
5�B
6B
6B
5�B
5?B
5ZB
6`B
6�B
6�B
7fB
72B
7�B
7�B
7�B
7�B
7�B
8B
9>B
9�B
9XB
9rB
:DB
:*B
:B
:B
:*B
:�B
;dB
<�B
=B
=�B
=�B
=�B
=�B
=�B
=�B
=�B
=�B
>B
>wB
>�B
?.B
@�B
@�B
A�B
B'B
BuB
B�B
B�B
B�B
B�B
CGB
C�B
DMB
D�B
D�B
DB
DMB
C�B
D�B
ESB
EmB
E9B
E�B
FB
F�B
G+B
G+B
G�B
G�B
G�B
G�B
G�B
G�B
H�B
H�B
IB
I�B
I�B
JXB
J�B
J�B
KB
K)B
K^B
K�B
K�B
LdB
LdB
L~B
L�B
MB
MPB
MjB
MjB
MjB
M�B
MPB
MjB
M�B
M�B
NVB
N�B
N�B
N�B
N�B
N�B
OBB
O\B
OBB
O�B
O�B
O�B
O�B
PB
P�B
P�B
QB
QhB
QhB
QhB
Q�B
QhB
Q�B
RB
RoB
RTB
RoB
RTB
R�B
RoB
R�B
R�B
R�B
R�B
S@B
S�B
S�B
S�B
TFB
T�B
T{B
T�B
T�B
UB
U2B
UMB
U�B
VB
VB
VSB
V9B
V�B
W?B
WsB
W�B
W�B
W�B
W�B
X+B
X+B
X+B
XB
XEB
XEB
X_B
X�B
X�B
X�B
X�B
X�B
Y1B
YeB
Y�B
ZB
ZB
Y�B
ZQB
[	B
[qB
[qB
[qB
[�B
[�B
[�B
[�B
\xB
\�B
\�B
\�B
]B
\�B
\�B
]B
]�B
]�B
^B
^B
^B
^B
]�B
^B
]�B
^B
_B
_!B
_pB
_�B
_�B
_�B
`B
`'B
`BB
`vB
`�B
`�B
`�B
aB
aHB
a�B
bhB
b�B
b�B
b�B
b�B
cB
b�B
b�B
c�B
cnB
c�B
c�B
dB
c�B
c�B
d&B
d&B
d�B
d�B
eB
e`B
eFB
e�B
e�B
e�B
ffB
f�B
f�B
ffB
ffB
f�B
g�B
g�B
g�B
h
B
h�B
h�B
h�B
h�B
h�B
iyB
i�B
i�B
i�B
jKB
j�B
kB
k6B
k�B
k�B
k�B
k�B
k�B
lWB
lqB
l�B
mB
mB
m)B
mB
m)B
mB
m)B
m)B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
ncB
n�B
n�B
n�B
n�B
oB
oB
oiB
oiB
o�B
o�B
o�B
poB
p�B
q'B
q'B
qAB
q[B
q[B
q[B
q�B
rB
raB
r�B
r�B
r�B
r�B
sMB
s�B
tB
tB
tB
t9B
t�B
u%B
u�B
vB
vFB
v�B
vzB
v`B
v�B
v�B
v�B
v�B
v�B
v�B
wB
wB
wB
wB
w�B
w�B
w�B
xB
xB
xB
xlB
x�B
x�B
x�B
y	B
y>B
yXB
yrB
y�B
z�B
z�B
{0B
{dB
{dB
{dB
{dB
{B
{�B
{�B
{�B
{�B
|B
|PB
|PB
|PB
|�B
|�B
|�B
|�B
}B
}�B
}q1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�;B��B�!B�UB�UB�UB�!B�B�!B�!B�!B�;B�B�B�!B�;B�;B�!B�!B�!B�!B�B�;B�!B�;B�UB�UB�UB�;B�B��B��B�B�!B�oB��B�oB�B�UB��B	�B	,�B	�B
y�B
�tB��B�|B��B�iB�B��B��B��B�;B�B��B��B��B�JB��B��BtTBX�B7�B"�B�B
�B
�B
ؓB
��B
��B
�gB
~�B
f2B
ZQB
PB
?cB
+B
�B	�B	��B	�	B	��B	��B	��B	zxB	m�B	_VB	KxB	33B	�B	B	�B	�B��B�B��B�^B	 �B	 �B��B�B��B��B	EB	�B	'�B	>B	G�B	K�B	GEB	J=B	O�B	\�B	a�B	i�B	y�B	�oB	�YB	��B	�B	��B	��B	�:B	��B	�B	�#B	��B	��B	�/B	��B	��B	��B	�B	�B	�wB	�B	�iB	�aB	�B	�@B	�B	��B
B
�B

�B
B
B
B
�B
�B
�B
)B
JB
�B
	7B
�B	�6B	�XB	�+B	��B	�%B	�|B	�-B	��B	��B
3B
GB	�B	�*B	�fB	��B	�/B	��B	�jB
 �B
�B
�B
�B
�B
�B
,B
B
	B
�B
	�B
zB
?B
 B	�JB	�B	�nB	�hB	�B	��B	�OB	��B	�B	�wB	�}B	��B	�GB	�B	��B	�6B	�VB	��B
  B
 iB
 B
'B
�B
�B
B
�B
�B
aB
�B
B
�B
�B
MB
B
MB
uB
�B
 B
 4B
 �B
 �B
B
�B

rB
	�B

	B

#B
	�B

	B
	�B
	�B
	�B
	RB
	B
�B
1B
�B
�B
�B
�B
EB
_B
B
EB
_B
EB
+B
�B
EB
�B
�B
tB
YB
�B
{B
;B	�wB	�0B	�	B	�B	��B	�PB	�xB	�$B	��B	�B	��B	��B	�}B	�B	��B	�wB	��B	��B	��B	��B	��B	�aB	��B	�B	�B	�B	�XB	��B	�dB	�B	�B	��B	�B	�JB	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�qB	�qB	�"B	��B	�B	�>B	�DB	��B	�xB	��B	��B	�]B	��B	�6B	��B	��B	��B	�	B	��B	��B	�	B	�>B	�rB	�rB	�XB	��B	�lB	�8B	�B	��B	�LB	�B	�zB	��B	�%B	�%B	�LB	�+B	��B	�B	�B	�3B	�B	�FB	��B	�+B	�B	��B	�aB	�-B	��B	�B	�B	�MB	�B	�B	�B	�B	��B	�B	��B	�zB	��B	�fB	�fB	�2B	�B	��B	�B	�2B	��B	��B	��B	��B	�	B	�B	�^B	��B	��B	��B	�>B	�rB	�B	�B	�xB	�DB	��B	��B	��B	��B	��B	�jB	��B	��B	��B	��B
B
 �B	�}B	�cB	�cB	�.B	�cB
 B
 4B
 �B
 B
;B
�B
�B
[B
�B
�B
�B
�B
B
GB
aB
�B
�B
B
B
�B
�B
[B
{B
�B
�B
�B
[B
oB
 4B	�}B	��B	�rB	�+B	��B	��B	�B	��B	�GB	��B	�'B	�B	�GB	�B	�^B	�B	�BB	�cB
 �B
UB
�B
�B
	7B
	�B
	�B

=B
xB
�B
JB
�B
�B
<B
�B
�B
�B
�B
�B
 B
�B
[B
FB
{B
�B
2B
SB
SB
mB

B
_B
�B
B
1B
1B
1B
1B
1B
1B
�B
kB
�B
�B
#B
=B
=B
#B
WB
�B
)B
]B
�B
�B
/B
IB
/B
/B
/B
/B
B
/B
dB
�B
jB
B
!B
�B
 �B
!B
!|B
!�B
!�B
!�B
"B
"�B
"�B
"�B
# B
# B
#nB
#�B
#�B
#�B
#�B
#�B
#�B
$�B
$�B
%�B
%�B
%�B
&�B
&�B
&�B
'8B
'�B
'�B
($B
($B
($B
'�B
'�B
'8B
(�B
(�B
)yB
*0B
)yB
)_B
)DB
(�B
(�B
(�B
)yB
)�B
)�B
)yB
)yB
)�B
)yB
)�B
*B
*�B
+B
*�B
*�B
+QB
+�B
+�B
+�B
+�B
+�B
+�B
,B
,"B
-)B
,�B
-)B
-CB
-]B
-]B
-wB
-wB
-wB
-�B
.IB
.IB
.�B
/OB
0�B
0�B
1�B
1vB
1�B
2B
2|B
2�B
3B
4TB
5B
5ZB
5ZB
5ZB
5tB
5�B
6B
6B
5�B
5?B
5ZB
6`B
6�B
6�B
7fB
72B
7�B
7�B
7�B
7�B
7�B
8B
9>B
9�B
9XB
9rB
:DB
:*B
:B
:B
:*B
:�B
;dB
<�B
=B
=�B
=�B
=�B
=�B
=�B
=�B
=�B
=�B
>B
>wB
>�B
?.B
@�B
@�B
A�B
B'B
BuB
B�B
B�B
B�B
B�B
CGB
C�B
DMB
D�B
D�B
DB
DMB
C�B
D�B
ESB
EmB
E9B
E�B
FB
F�B
G+B
G+B
G�B
G�B
G�B
G�B
G�B
G�B
H�B
H�B
IB
I�B
I�B
JXB
J�B
J�B
KB
K)B
K^B
K�B
K�B
LdB
LdB
L~B
L�B
MB
MPB
MjB
MjB
MjB
M�B
MPB
MjB
M�B
M�B
NVB
N�B
N�B
N�B
N�B
N�B
OBB
O\B
OBB
O�B
O�B
O�B
O�B
PB
P�B
P�B
QB
QhB
QhB
QhB
Q�B
QhB
Q�B
RB
RoB
RTB
RoB
RTB
R�B
RoB
R�B
R�B
R�B
R�B
S@B
S�B
S�B
S�B
TFB
T�B
T{B
T�B
T�B
UB
U2B
UMB
U�B
VB
VB
VSB
V9B
V�B
W?B
WsB
W�B
W�B
W�B
W�B
X+B
X+B
X+B
XB
XEB
XEB
X_B
X�B
X�B
X�B
X�B
X�B
Y1B
YeB
Y�B
ZB
ZB
Y�B
ZQB
[	B
[qB
[qB
[qB
[�B
[�B
[�B
[�B
\xB
\�B
\�B
\�B
]B
\�B
\�B
]B
]�B
]�B
^B
^B
^B
^B
]�B
^B
]�B
^B
_B
_!B
_pB
_�B
_�B
_�B
`B
`'B
`BB
`vB
`�B
`�B
`�B
aB
aHB
a�B
bhB
b�B
b�B
b�B
b�B
cB
b�B
b�B
c�B
cnB
c�B
c�B
dB
c�B
c�B
d&B
d&B
d�B
d�B
eB
e`B
eFB
e�B
e�B
e�B
ffB
f�B
f�B
ffB
ffB
f�B
g�B
g�B
g�B
h
B
h�B
h�B
h�B
h�B
h�B
iyB
i�B
i�B
i�B
jKB
j�B
kB
k6B
k�B
k�B
k�B
k�B
k�B
lWB
lqB
l�B
mB
mB
m)B
mB
m)B
mB
m)B
m)B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
ncB
n�B
n�B
n�B
n�B
oB
oB
oiB
oiB
o�B
o�B
o�B
poB
p�B
q'B
q'B
qAB
q[B
q[B
q[B
q�B
rB
raB
r�B
r�B
r�B
r�B
sMB
s�B
tB
tB
tB
t9B
t�B
u%B
u�B
vB
vFB
v�B
vzB
v`B
v�B
v�B
v�B
v�B
v�B
v�B
wB
wB
wB
wB
w�B
w�B
w�B
xB
xB
xB
xlB
x�B
x�B
x�B
y	B
y>B
yXB
yrB
y�B
z�B
z�B
{0B
{dB
{dB
{dB
{dB
{B
{�B
{�B
{�B
{�B
|B
|PB
|PB
|PB
|�B
|�B
|�B
|�B
}B
}�B
}q1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104933  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604174409  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604174410  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604174410                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605024417  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605024417  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610141505                      G�O�G�O�G�O�                