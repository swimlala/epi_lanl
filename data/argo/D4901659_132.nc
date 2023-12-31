CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-03-11T17:03:32Z creation      
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
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  `�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  p    TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �H   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �0   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �p   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �X   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ݀   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ݰ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �,   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �<   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �@   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �P   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �T   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �X   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �\        �\Argo profile    3.1 1.2 19500101000000  20180311170332  20230721230915  4901659 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               �A   AO  5382                            2C  D   NAVIS_A                         0371                            082713                          863 @�R��LH�1   @�R��8�@@<����o�c�|�hs1   GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                      �A   A   A   @9��@�  @�  A   A   A@  A`  A���A�  A�  A�  A�  A�  A�33A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CS�fCV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#fD#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DLy�DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]�fD^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�3D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�<�D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D��3D��f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@4z�@z�H@�p�@�p�A�RA>�RA^�RA�(�A�\)A�\)A�\)A�\)A�\)Aޏ\A�\)A�\)B�B�B�B�B'�B/�B7�B?�BG�BO�BW�B_�Bg�Bo�Bw�B�B��
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
C�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS��CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D z�D ��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��D	z�D	��D
z�D
��Dt{D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��D z�D ��D!z�D!��D"z�D#GD#z�D#��D$z�D$��D%z�D%��D&z�D&��D'z�D'��D(z�D(��D)z�D)��D*z�D*��D+z�D+��D,z�D,��D-z�D-��D.z�D.��D/z�D/��D0z�D0��D1z�D1��D2z�D2��D3z�D3��D4z�D4��D5z�D5��D6z�D6��D7z�D7��D8z�D8��D9z�D9��D:z�D:��D;z�D;��D<z�D<��D=z�D=��D>z�D>��D?z�D?��D@z�D@��DAz�DA��DBz�DB��DCz�DC��DDz�DD��DEz�DE��DFz�DF��DGz�DG��DHz�DH��DIz�DI��DJz�DJ��DKz�DK��DLt{DL��DMz�DM��DNz�DN��DOz�DO��DPz�DP��DQz�DQ��DRz�DR��DSz�DS��DTz�DT��DUz�DU��DVz�DV��DWz�DW��DXz�DX��DYz�DY��DZz�DZ��D[z�D[��D\z�D\��D]�GD]��D^z�D^��D_z�D_��D`z�D`��Daz�Da��Dbz�Db��Dcz�Dc��Ddz�Dd��Dez�De��Dfz�Df��Dgz�Dg��Dhz�Dh��Diz�Di��Djz�Dj��Dkz�Dk��Dlz�Dl��Dmz�Dm��Dnz�Dn��Doz�Do��Dpz�Dp��Dqz�Dq��Drz�Dr��Dsz�Ds��Dtz�Dt��Duz�Du��Dvz�Dv��Dwz�Dw��Dxz�Dx��Dyz�Dy��Dzz�Dz��D{z�D{��D|z�D|��D}z�D}��D~z�D~��Dz�D��D�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD���D��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD���D��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD½qD��qD�=qD�}qDýqD��qD�=qD�}qDĽqD��qD�=qD�}qDŽqD��qD�=qD�}qDƽqD��qD�=qD�}qDǽqD��qD�=qD�}qDȽqD��qD�=qD�}qDɽqD��qD�=qD�}qDʽqD��qD�=qD�}qD˽qD��qD�=qD�}qD̽qD��qD�=qD�}qDͽqD��qD�=qD�}qDνqD��qD�=qD�}qDϽqD��qD�=qD�}qDнqD��qD�=qD�}qDѽqD��qD�=qD�}qDҽqD��qD�=qD�}qDӽqD� �D�=qD�}qDԽqD��qD�=qD�}qDսqD��qD�=qD�}qDֽqD��qD�=qD�}qD׽qD��qD�=qD�}qDؽqD��qD�=qD�}qDٽqD��qD�=qD�}qDڽqD��qD�=qD�}qD۽qD��qD�=qD�}qDܽqD��qD�=qD�}qDݽqD��qD�=qD�}qD޽qD��qD�=qD�}qD߽qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�:>D�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD��qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD���D���D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��;A��HA��;A��TA��`A��`A��TA��;A��HA��;A��/A��yA��mA��A���A���A���A���A�~�A�A�A��
A�\)A�7LA�/A�+A� �A�{A�%A���A���A��!A���A���A��PA��7A��A�~�A�r�A�r�A�p�A�p�A�p�A�p�A�jA�dZA�^5A��A��wA���A���A�A��9A�bA��;A��RA�dZA�A�jA��A���A�hsA�p�A�oA��A�hsA�{A�A�ȴA�ZA�JA�  A�v�A�r�A��`A�`BA���A��/A���A�?}A���A��PA�v�A�z�A�1'A�p�A�9XA�t�A��wA�A�ffA���A�Q�A��9A�^5A�O�A���A�5?A���A�v�A��!A�;dA���A���A� �A��RA�A�jA��A~I�A{�Ay�;Ax�+Av�DAv-Au�Au��At5?As/Ar��Arn�Ar(�Aq��Ap1Ao�hAn�DAlȴAl �AjbNAfffAdffAbr�Aa�-Aa�A`�A_�mA^��A]�
A]`BA\1AYoAV�RAU�7AT��AS��AR��AQ�TAP�RAP�AM��AL��AL$�AKdZAK+AJ�AJM�AIoAH�AG��AG"�AF-AD1AB��AB�AA�wA?�;A=��A=��A=hsA<��A<{A;�-A;�hA;7LA9�A8�\A8Q�A8�A7VA6VA4n�A4  A3G�A2�/A2��A2��A2v�A25?A1|�A0ĜA.�/A-A-
=A,r�A+7LA*=qA)`BA(�A'��A'%A&z�A%x�A%K�A$�/A$�A#K�A"5?A!VAp�A�!AJA��A^5A-A��AĜAQ�AE�AA�PAO�A��A�DA�FA�!AE�A1'A  A��A?}An�A"�A��A��AI�A33A%A�A�A�\A�;Az�A|�AoA	�A^5Ax�AM�A�Ax�A;dAZA&�A �+A �@��;@�t�@��@��@�1'@��@��!@�n�@�{@���@�7L@��u@��h@�Ĝ@��@�l�@�
=@�R@�hs@�K�@�
=@�V@�|�@��`@���@��@��m@�n�@�r�@�=q@ݙ�@�`B@ܼj@�ƨ@ؓu@�ff@��
@�~�@���@�I�@�1@���@�+@ΰ!@�M�@�(�@�v�@ɩ�@�7L@���@�bN@���@ǝ�@��@Ɨ�@�V@�@�hs@�9X@Õ�@���@§�@�@���@�9X@��w@��@�?}@��@��@�9X@�33@���@��7@�1@�"�@���@��@�M�@���@��@��@�+@��9@�Z@���@�X@�X@���@���@��/@�A�@�|�@��T@��@��@�p�@��y@��#@�X@�V@�%@��@���@��D@��u@�j@�Q�@�(�@�dZ@��T@�x�@��j@�b@�\)@�v�@��T@��@�b@�@��@���@��@�`B@�?}@�V@���@�S�@�
=@��y@��@��@��-@�p�@�V@���@�j@��w@�@�^5@���@�X@�&�@���@���@�z�@�(�@���@���@�ƨ@��@��F@���@�|�@��@��@��H@�ff@���@�x�@�%@��/@��j@�9X@���@��;@�l�@��@�@���@�5?@�@�x�@�X@�G�@��@��@��`@��9@�(�@��@��
@���@�l�@�33@��@���@�^5@�E�@�-@��@�hs@�7L@��@���@���@��@�(�@�w@�P@l�@~ff@}�T@}@}/@|�j@{�m@z�H@z~�@y��@y��@yx�@xĜ@xQ�@xb@w�@w�w@wK�@w
=@v�@v��@vE�@v{@u�T@u@u��@u?}@t��@tz�@t(�@s��@s�m@s�
@s�
@sƨ@s��@sS�@r��@r~�@r^5@r=q@q�^@q7L@pA�@p �@pQ�@p��@q7L@q�@p�`@p�9@pr�@o�w@n�@mp�@l�@l��@l�/@m/@m?}@m?}@m?}@m`B@mp�@m`B@lZ@kdZ@kt�@k33@k33@j�@j��@j~�@i��@ihs@i�@h  @g�w@gK�@f�y@f��@fV@f@e�T@e�T@e�-@e�@e/@d�D@d(�@c��@c��@ct�@b��@b��@b�\@bJ@`��@`Q�@` �@`  @_�@_K�@^�@^5?@]�@]�T@]`B@]V@\�j@\I�@[��@[�m@[�F@[��@[�@[dZ@[C�@[33@Z��@Y��@Yhs@Y%@XbN@X  @W�@W��@V�y@Vȴ@V�+@U�@U@Up�@T��@S�m@St�@SS�@S"�@R�@R�\@Q�#@Q��@Q��@Q�7@Qx�@QG�@Q�@P�`@P�9@P��@P�u@P1'@O��@Nȴ@N5?@N@M@Mp�@M/@MV@L�@L�@Lz�@L1@K�F@K��@K��@Kt�@J~�@J=q@J=q@J�@I��@I�@I�^@I�7@IX@I&�@I%@H��@H��@H�u@H�@HbN@HQ�@H �@H1'@H1'@H  @G�;@G��@F�y@Fȴ@Fv�@FV@FE�@FE�@F$�@E��@E��@E�@EO�@EV@D��@D�/@D��@DZ@D�@C�m@C�
@Cƨ@C�F@C�@Co@B�!@BM�@B-@BJ@A�@A��@A%@@��@@r�@@bN@@bN@@bN@@Q�@@Q�@@A�@@  @?l�@?�@>�+@>5?@=�@=�@<�@<�/@<�@<j@<(�@;�
@;�@;33@;o@:��@:~�@:=q@:=q@:-@9�@9hs@8��@8�@7�P@7+@6�@6ȴ@6V@5��@4�@4��@4�D@4z�@4j@4Z@4(�@3��@3�m@3ƨ@3��@3t�@3dZ@3dZ@3dZ@3dZ@3dZ@3C�@333@2�@2�H@2�H@2�@3@2�@2^5@2�@1��@1��@1��@1�7@1x�@1X@17L@1�@0��@0�`@0��@0Ĝ@0�@0bN@/�@/|�@/K�@.�y@.�R@.{@-�-@-�h@-p�@-?}@,��@,�/@,��@,�@+�F@+C�@+o@*��@*�!@*^5@*=q@*J@)�#@)�^@)�7@)hs@)�@)%@(��@(�9@(�9@(�u@(r�@(Q�@(1'@'�;@'�;@'��@'��@'\)@';d@'�@'
=@&��@&�R@&V@&@%@%p�@%V@$��@$��@$�j@$�j@$�@$��@$9X@#�
@#C�@"��@"��@"�\@"~�@"M�@"J@!��@!��@!��@!7L@ �`@ Ĝ@ ��@ r�@ Q�@ A�@   @�;@��@��@�w@�@��@�P@�P@�P@|�@l�@l�@\)@\)@\)@K�@K�@
=@�@�R@��@v�@5?@@�T@�-@��@�D@�D@z�@Z@��@�@"�@@�H@��@��@n�@^5@J@�#@��@��@��@��@��@��@�7@G�@7L@%@�`@��@�9@�@A�@�@��@�@|�@l�@\)@K�@�y@��@V@{@@@��@��@p�@O�@O�@O�@?}@�@V@�/@�@�@��@j@I�@9X@�@ƨ@33@�H@��@n�@�#@��@��@�7@�7@hs@X@&�@%@�`@�@A�@1'@  @��@�P@|�@\)@;d@
=@��@ȴ@��@v�@$�@��@�-@�h@O�@V@�@�j@z�@j@I�@I�@9X@9X@(�@1@�
@�F@�@C�@C�@C�@
�H@
~�@
�@
�@	��@	��@	��@	x�@	X@	G�@	7L@	�@��@�`@�`@��@Ĝ@Ĝ@�9@�9@�9@��@��@�u@r�@ �@��@�@�@�P@l�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��;A��HA��;A��TA��`A��`A��TA��;A��HA��;A��/A��yA��mA��A���A���A���A���A�~�A�A�A��
A�\)A�7LA�/A�+A� �A�{A�%A���A���A��!A���A���A��PA��7A��A�~�A�r�A�r�A�p�A�p�A�p�A�p�A�jA�dZA�^5A��A��wA���A���A�A��9A�bA��;A��RA�dZA�A�jA��A���A�hsA�p�A�oA��A�hsA�{A�A�ȴA�ZA�JA�  A�v�A�r�A��`A�`BA���A��/A���A�?}A���A��PA�v�A�z�A�1'A�p�A�9XA�t�A��wA�A�ffA���A�Q�A��9A�^5A�O�A���A�5?A���A�v�A��!A�;dA���A���A� �A��RA�A�jA��A~I�A{�Ay�;Ax�+Av�DAv-Au�Au��At5?As/Ar��Arn�Ar(�Aq��Ap1Ao�hAn�DAlȴAl �AjbNAfffAdffAbr�Aa�-Aa�A`�A_�mA^��A]�
A]`BA\1AYoAV�RAU�7AT��AS��AR��AQ�TAP�RAP�AM��AL��AL$�AKdZAK+AJ�AJM�AIoAH�AG��AG"�AF-AD1AB��AB�AA�wA?�;A=��A=��A=hsA<��A<{A;�-A;�hA;7LA9�A8�\A8Q�A8�A7VA6VA4n�A4  A3G�A2�/A2��A2��A2v�A25?A1|�A0ĜA.�/A-A-
=A,r�A+7LA*=qA)`BA(�A'��A'%A&z�A%x�A%K�A$�/A$�A#K�A"5?A!VAp�A�!AJA��A^5A-A��AĜAQ�AE�AA�PAO�A��A�DA�FA�!AE�A1'A  A��A?}An�A"�A��A��AI�A33A%A�A�A�\A�;Az�A|�AoA	�A^5Ax�AM�A�Ax�A;dAZA&�A �+A �@��;@�t�@��@��@�1'@��@��!@�n�@�{@���@�7L@��u@��h@�Ĝ@��@�l�@�
=@�R@�hs@�K�@�
=@�V@�|�@��`@���@��@��m@�n�@�r�@�=q@ݙ�@�`B@ܼj@�ƨ@ؓu@�ff@��
@�~�@���@�I�@�1@���@�+@ΰ!@�M�@�(�@�v�@ɩ�@�7L@���@�bN@���@ǝ�@��@Ɨ�@�V@�@�hs@�9X@Õ�@���@§�@�@���@�9X@��w@��@�?}@��@��@�9X@�33@���@��7@�1@�"�@���@��@�M�@���@��@��@�+@��9@�Z@���@�X@�X@���@���@��/@�A�@�|�@��T@��@��@�p�@��y@��#@�X@�V@�%@��@���@��D@��u@�j@�Q�@�(�@�dZ@��T@�x�@��j@�b@�\)@�v�@��T@��@�b@�@��@���@��@�`B@�?}@�V@���@�S�@�
=@��y@��@��@��-@�p�@�V@���@�j@��w@�@�^5@���@�X@�&�@���@���@�z�@�(�@���@���@�ƨ@��@��F@���@�|�@��@��@��H@�ff@���@�x�@�%@��/@��j@�9X@���@��;@�l�@��@�@���@�5?@�@�x�@�X@�G�@��@��@��`@��9@�(�@��@��
@���@�l�@�33@��@���@�^5@�E�@�-@��@�hs@�7L@��@���@���@��@�(�@�w@�P@l�@~ff@}�T@}@}/@|�j@{�m@z�H@z~�@y��@y��@yx�@xĜ@xQ�@xb@w�@w�w@wK�@w
=@v�@v��@vE�@v{@u�T@u@u��@u?}@t��@tz�@t(�@s��@s�m@s�
@s�
@sƨ@s��@sS�@r��@r~�@r^5@r=q@q�^@q7L@pA�@p �@pQ�@p��@q7L@q�@p�`@p�9@pr�@o�w@n�@mp�@l�@l��@l�/@m/@m?}@m?}@m?}@m`B@mp�@m`B@lZ@kdZ@kt�@k33@k33@j�@j��@j~�@i��@ihs@i�@h  @g�w@gK�@f�y@f��@fV@f@e�T@e�T@e�-@e�@e/@d�D@d(�@c��@c��@ct�@b��@b��@b�\@bJ@`��@`Q�@` �@`  @_�@_K�@^�@^5?@]�@]�T@]`B@]V@\�j@\I�@[��@[�m@[�F@[��@[�@[dZ@[C�@[33@Z��@Y��@Yhs@Y%@XbN@X  @W�@W��@V�y@Vȴ@V�+@U�@U@Up�@T��@S�m@St�@SS�@S"�@R�@R�\@Q�#@Q��@Q��@Q�7@Qx�@QG�@Q�@P�`@P�9@P��@P�u@P1'@O��@Nȴ@N5?@N@M@Mp�@M/@MV@L�@L�@Lz�@L1@K�F@K��@K��@Kt�@J~�@J=q@J=q@J�@I��@I�@I�^@I�7@IX@I&�@I%@H��@H��@H�u@H�@HbN@HQ�@H �@H1'@H1'@H  @G�;@G��@F�y@Fȴ@Fv�@FV@FE�@FE�@F$�@E��@E��@E�@EO�@EV@D��@D�/@D��@DZ@D�@C�m@C�
@Cƨ@C�F@C�@Co@B�!@BM�@B-@BJ@A�@A��@A%@@��@@r�@@bN@@bN@@bN@@Q�@@Q�@@A�@@  @?l�@?�@>�+@>5?@=�@=�@<�@<�/@<�@<j@<(�@;�
@;�@;33@;o@:��@:~�@:=q@:=q@:-@9�@9hs@8��@8�@7�P@7+@6�@6ȴ@6V@5��@4�@4��@4�D@4z�@4j@4Z@4(�@3��@3�m@3ƨ@3��@3t�@3dZ@3dZ@3dZ@3dZ@3dZ@3C�@333@2�@2�H@2�H@2�@3@2�@2^5@2�@1��@1��@1��@1�7@1x�@1X@17L@1�@0��@0�`@0��@0Ĝ@0�@0bN@/�@/|�@/K�@.�y@.�R@.{@-�-@-�h@-p�@-?}@,��@,�/@,��@,�@+�F@+C�@+o@*��@*�!@*^5@*=q@*J@)�#@)�^@)�7@)hs@)�@)%@(��@(�9@(�9@(�u@(r�@(Q�@(1'@'�;@'�;@'��@'��@'\)@';d@'�@'
=@&��@&�R@&V@&@%@%p�@%V@$��@$��@$�j@$�j@$�@$��@$9X@#�
@#C�@"��@"��@"�\@"~�@"M�@"J@!��@!��@!��@!7L@ �`@ Ĝ@ ��@ r�@ Q�@ A�@   @�;@��@��@�w@�@��@�P@�P@�P@|�@l�@l�@\)@\)@\)@K�@K�@
=@�@�R@��@v�@5?@@�T@�-@��@�D@�D@z�@Z@��@�@"�@@�H@��@��@n�@^5@J@�#@��@��@��@��@��@��@�7@G�@7L@%@�`@��@�9@�@A�@�@��@�@|�@l�@\)@K�@�y@��@V@{@@@��@��@p�@O�@O�@O�@?}@�@V@�/@�@�@��@j@I�@9X@�@ƨ@33@�H@��@n�@�#@��@��@�7@�7@hs@X@&�@%@�`@�@A�@1'@  @��@�P@|�@\)@;d@
=@��@ȴ@��@v�@$�@��@�-@�h@O�@V@�@�j@z�@j@I�@I�@9X@9X@(�@1@�
@�F@�@C�@C�@C�@
�H@
~�@
�@
�@	��@	��@	��@	x�@	X@	G�@	7L@	�@��@�`@�`@��@Ĝ@Ĝ@�9@�9@�9@��@��@�u@r�@ �@��@�@�@�P@l�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�9B�9B�9B�9B�9B�9B�9B�9B�9B�?B�9B�9B�9B�9B�9B�9B�9B�9B�9B�9B�9B�?B�?B�?B�FB�FB�FB�FB�FB�FB�LB�LB�LB�LB�RB�RB�RB�RB�RB�RB�RB�RB�RB�RB�LB�LB�FB�RB�LB�jB�LB�9B�!B�9B�9B�B��B��B��B�{B�JB{�Bv�Bs�Bp�Br�Bs�Bo�Bs�Bt�BN�B?}B%�B�B%B��B��B��B
=BVBVBVBoBVB  B�B�)B��B�}B�'B��B��B�JBu�B`BB>wBoB
��B
�BB
��B
ɺB
��B
�FB
�B
��B
��B
�bB
�1B
}�B
l�B
]/B
P�B
D�B
A�B
?}B
=qB
49B
,B
)�B
&�B
#�B
�B
�B
�B
bB
B	��B	�B	��B	�wB	�-B	�B	��B	��B	��B	��B	�{B	�bB	�1B	z�B	p�B	jB	gmB	bNB	]/B	YB	S�B	M�B	B�B	;dB	9XB	;dB	;dB	9XB	5?B	33B	/B	,B	)�B	%�B	�B	bB	PB	1B��B�B��B��B��B�B�B�B�B�HB�)B�B�
B��B��B��BŢB��B�wB�wB�wB��B�wB�dB�?B�B��B��B��B��B�bB�JB�=B�B�B� B|�B{�By�Bw�Bt�Bp�Bm�BjBiyBhsBffBe`BdZBcTBbNBaHB`BB`BB^5B]/B\)B[#BYBXBXBXBW
BVBT�BR�BR�BQ�BO�BP�BP�BP�BQ�BO�BO�BM�BL�BJ�BH�BG�BE�BC�BB�B@�B?}B>wB=qB>wB>wB=qB<jB<jB<jB<jB;dB:^B;dB:^B:^B:^B9XB8RB:^B9XB9XB9XB9XB8RB7LB8RB7LB6FB5?B5?B5?B6FB6FB5?B49B6FB6FB6FB7LB7LB8RB9XB;dB;dB=qB=qB>wB=qB>wB?}B?}BB�BE�BF�BF�BG�BG�BH�BH�BI�BJ�BJ�BJ�BK�BM�BM�BN�BN�BO�BQ�BR�BQ�BXBYBYB[#B\)B_;B`BBaHB]/B]/B[#BYBYBW
BVBS�BP�BP�BR�BVBZB\)B[#B[#B^5B`BB`BB^5BhsBz�B}�By�Bx�Bk�BaHB_;B_;BbNBcTBdZBe`BffBffBhsBl�Bn�Bq�Bt�Bv�Bz�B|�B� B�B�=B�DB�JB�VB�oB�oB�uB��B��B��B��B��B��B��B��B��B�B�!B�3B�LB�jBBŢBƨBǮBǮBɺB��B��B��B��B��B��B��B��B��B��B��B�B�/B�;B�NB�ZB�ZB�sB�B�B�B�B�B��B��B��B��B	  B	  B	B	B	B	B	1B	
=B	DB	PB	VB	bB	oB	�B	�B	�B	�B	�B	�B	!�B	!�B	"�B	$�B	&�B	+B	-B	.B	/B	49B	7LB	8RB	;dB	=qB	A�B	E�B	G�B	J�B	K�B	L�B	O�B	Q�B	S�B	S�B	VB	YB	ZB	[#B	]/B	`BB	aHB	cTB	dZB	e`B	hsB	m�B	m�B	o�B	p�B	p�B	q�B	q�B	r�B	s�B	t�B	u�B	v�B	v�B	v�B	v�B	w�B	z�B	|�B	}�B	� B	�B	�%B	�1B	�7B	�=B	�PB	�\B	�bB	�bB	�hB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�3B	�?B	�?B	�LB	�RB	�RB	�RB	�RB	�XB	�^B	�dB	�jB	�jB	�qB	�wB	��B	��B	��B	��B	B	ĜB	ĜB	ŢB	ƨB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�#B	�/B	�5B	�BB	�HB	�HB	�NB	�`B	�fB	�mB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
+B
+B
+B
+B
+B
1B
	7B

=B

=B

=B

=B
DB
DB
JB
JB
PB
PB
PB
VB
VB
VB
VB
bB
hB
oB
uB
uB
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
!�B
!�B
!�B
"�B
#�B
#�B
$�B
$�B
%�B
%�B
&�B
&�B
&�B
&�B
'�B
'�B
(�B
)�B
+B
,B
-B
-B
.B
/B
1'B
1'B
1'B
1'B
1'B
1'B
1'B
2-B
2-B
2-B
2-B
2-B
2-B
2-B
2-B
2-B
2-B
2-B
2-B
33B
33B
33B
33B
33B
2-B
33B
33B
49B
49B
49B
49B
5?B
5?B
5?B
5?B
6FB
6FB
6FB
6FB
7LB
7LB
8RB
9XB
9XB
:^B
:^B
;dB
;dB
<jB
<jB
<jB
<jB
=qB
=qB
=qB
>wB
?}B
@�B
@�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
F�B
F�B
G�B
G�B
H�B
H�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
K�B
L�B
L�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
VB
VB
VB
VB
VB
W
B
W
B
XB
XB
XB
XB
YB
YB
YB
YB
ZB
ZB
ZB
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
\)B
]/B
]/B
]/B
]/B
]/B
^5B
^5B
_;B
_;B
_;B
_;B
_;B
_;B
`BB
`BB
`BB
`BB
`BB
`BB
`BB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
bNB
cTB
cTB
cTB
dZB
e`B
e`B
e`B
e`B
e`B
e`B
e`B
ffB
ffB
ffB
ffB
gmB
gmB
gmB
gmB
hsB
hsB
hsB
hsB
iyB
hsB
iyB
iyB
iyB
jB
jB
jB
jB
k�B
k�B
k�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
r�B
q�B
r�B
r�B
q�B
r�B
r�B
s�B
s�B
s�B
s�B
s�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�+B�JB�1B�6B�DB�MB�FB�9B�JB�LB�B�DB�"B�+B��B�XB�B�B�|B�:B�vB��B�lB�[B��B��B��B��B�B��B��B�wB��B�hB�wB�pB��B�[B�bB�[B�WB�^B�wB�vB��B��B��B�&B�zB�B��B�3B�B�B�B�.B��B�jB�B�B�AB}�Bx�BuLBrfBs*BuBr2BvHB�?BRABEB)BmB;B��B�ZB�[B�B�B�BOB�B]BrB��B��BѧB��B�KB��B��B�7ByuBfIBGrB*BB
�B
�B
̥B
�ZB
��B
�B
�OB
��B
��B
�B
�oB
r>B
`�B
V2B
E�B
BMB
@�B
AVB
7 B
-B
+B
'�B
%�B
#�B
B
_B
�B
,B
�B	��B	�>B	��B	�8B	��B	��B	��B	��B	�2B	��B	��B	��B	hB	r�B	k�B	iB	d�B	_#B	[aB	U:B	R(B	EoB	<WB	:�B	;�B	;�B	:�B	7�B	5�B	0cB	-EB	,�B	++B	#B	�B	^B	�B�JB�B�?B��B�B�B�B�B��B�B��BٺBٛB��BӓB��BǌB��B��B��B�9B�<B�mB��B�wB��B��B�xB��B�,B��B��B��B��B��B��B}�B}7B{B{jBw�Bt1BrBl�Bk�Bl	Bg�BfBe�BfDBc�BaBaBa�B^�B^�B]B]�B\BY<BX\BX�BW�BW�BW�BV�BToBS�BTpBS�BQeBQBQ�BQMBQ�BQ.BN�BK�BK�BJ�BG�BF:BE}BDoB@B@�B@[B@B?�B=�B=B>�B=�B<�B<WB;|B;�B:�B:�B;B:ZB<BB;�B:OB:BB9�B9�B:%B:-B8�B8GB9�B8uB8B7�B7�B8<B7�B6�B7B6�B7B8�B;B;B<�B=B=VB>bB=�B>�B>NB?/B@+BB�BEBF�BG_BG[BH_BH`BIYBI�BJ�BK;BK_BK�BM�BN�BN�BOwBPBQ�BSBS�BT�BY?BY�BY�B\B]�B`lBbWBc�B_Bb�B]BZFBZWBW�BV�BV�BUBQ{BRtBT�BZ7B]B[.B['B_JBa�Bb�B]�Bc�Bz�B��B|B�Bp=BcTB_qB_�Bb}BcUBd�Be�Bf�BgbBjQBmBo�Br�Bu�Bw�B{�B~'B�B�fB�{B��B��B�TB��B��B� B��B� B��B��B��B�|B�TB��B�mB��B�CB�aB�lB�dB�VB��B��B�B�RB�HB�'B� B��B�B��B�B�/BГB�JB�2B��B�*B��B�B�B�B�?B��B�B�RB�5B��B�sB��B��B�qB�:B	 *B	 TB	iB	4B	xB	�B	�B	
uB	�B	�B	�B	�B	B	�B	�B	�B	�B	UB	 B	!�B	!�B	#B	%@B	'^B	+eB	-6B	.6B	/�B	4�B	7qB	8�B	;�B	>B	BCB	E�B	HB	KB	K�B	MPB	P8B	R B	TB	T&B	VZB	YPB	ZMB	[\B	]vB	`mB	auB	cvB	dB	e�B	h�B	m�B	m�B	o�B	p�B	p�B	q�B	q�B	r�B	s�B	u*B	vB	v�B	v�B	w9B	w<B	x�B	{B	|�B	}�B	�B	�5B	�TB	�gB	�zB	��B	�B	�{B	��B	��B	�eB	�;B	�tB	��B	��B	�xB	��B	��B	�tB	�tB	��B	��B	��B	�B	�)B	�B	��B	�B	�SB	��B	�WB	��B	��B	��B	��B	��B	�wB	�`B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	�eB	�B	��B	��B	��B	��B	�B	�CB	�B	��B	�=B	�%B	�*B	�LB	�>B	�B	�2B	�#B	�B	�)B	�'B	�!B	�oB	��B	ۀB	݁B	޴B	��B	�`B	�B	��B	�B	�B	��B	�B	��B	��B	�BB	��B	�B	��B	��B	��B	�"B	��B	�B	��B	��B	��B	��B	� B	��B	��B	��B	�/B	�=B	��B	�\B	�B	�'B	�<B	�/B	�B	�#B
 :B
 2B
fB
RB
+B
B
,B
�B
AB
B
.B
2B
&B
<B
<B
JB
IB
DB
MB
SB
BB
BB
IB
>B
SB
2B
	BB

gB

_B

zB

�B
fB
�B
jB
]B
^B
nB
�B
�B
vB
�B
�B
wB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
B
�B
@B
 B
 �B
!�B
!�B
"B
#	B
$B
$B
% B
% B
&-B
&B
'B
&�B
' B
'&B
([B
(SB
)_B
*�B
+VB
,QB
-(B
-sB
.�B
/�B
1>B
1>B
1>B
1@B
1@B
1YB
1SB
2DB
2QB
2SB
2^B
2DB
27B
29B
29B
2<B
2SB
2HB
2iB
3MB
3;B
3.B
30B
3MB
2�B
3rB
3ZB
4�B
4OB
4SB
4OB
5eB
5aB
5eB
5dB
6]B
6\B
6bB
6�B
7vB
7�B
8�B
9�B
9�B
:�B
:�B
;�B
;�B
<�B
<�B
<�B
<�B
=�B
=�B
>B
>�B
?�B
@�B
@�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
F�B
F�B
G�B
G�B
H�B
IB
I�B
I�B
I�B
I�B
J�B
J�B
KB
KB
K:B
L/B
L�B
L�B
M�B
NB
NB
NB
N�B
N�B
O/B
OB
P B
PB
PB
QB
P�B
QB
QB
P�B
P�B
RB
R B
RB
RB
Q�B
Q�B
RB
RB
Q�B
RB
Q�B
Q�B
RB
Q�B
R*B
S$B
SB
S
B
S&B
S.B
T%B
TB
T-B
T�B
V?B
VB
VB
V-B
V[B
WiB
WZB
X5B
X6B
X(B
XAB
YIB
Y2B
YfB
YHB
Z4B
Z'B
Z'B
Z(B
Z'B
Z'B
Z\B
ZXB
[;B
[UB
[HB
[=B
[HB
[WB
[aB
\xB
\LB
\PB
]`B
]FB
]IB
]KB
]�B
^sB
^�B
_xB
_PB
_JB
_oB
_pB
_nB
`fB
`MB
`MB
`YB
`fB
`\B
`sB
a|B
aSB
acB
a}B
aqB
aaB
apB
a�B
b�B
c�B
c�B
c�B
d�B
e�B
ekB
eyB
emB
e�B
eyB
e�B
f�B
f�B
f�B
f�B
g�B
g�B
g�B
g�B
h�B
h�B
h�B
h�B
i�B
h�B
i�B
i�B
i�B
j�B
j�B
j�B
j�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
r�B
q�B
r�B
r�B
q�B
r�B
r�B
s�B
s�B
s�B
s�B
s�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<(-<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<I�c<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<%fD<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<9�9<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - (REPORTED_SURFACE_PRESSURE) for PADJ                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al,2007,JAOT & effects of pressure adjustments                                                                                                                                                            PADJ REPORTED_SURFACE_PRESSURE =0.08 dbar                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CTM alpha = 0.141 & tau = 6.68 s with error equal to the correction                                                                                                                                                                                             Pressures adjusted using reported surface pressure when warranted.                                                                                                                                                                                              none                                                                                                                                                                                                                                                            Salinity corrected for pressure sensor calibration drift and cell thermal mass                                                                                                                                                                                  201810310930142018103109301420181031093014  AO  ARCAADJP                                                                    20180311170332    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20180311170332  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20180311170332  QCF$                G�O�G�O�G�O�0               PM  ARSQPADJV1.1                                                                20181031093014  QC  PRES            @9��D��fG�O�                PM  ARSQCTM V1.1                                                                20181031093014  QC  PSAL            @9��D��fG�O�                PM  ARSQCOWGV1.1CTD_2021v2 + Argo_2021v03                                       20230721230915  IP                  G�O�G�O�G�O�                