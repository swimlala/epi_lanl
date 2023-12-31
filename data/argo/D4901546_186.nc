CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-02-26T18:02:53Z creation      
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
_FillValue                 �  I    PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  `�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  pX   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �@   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ʌ   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �x   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �(   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �X   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �X   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �X   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �X   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �    HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �        �Argo profile    3.1 1.2 19500101000000  20180226180253  20181023151222  4901546 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               �A   AO  4737                            2C  D   NAVIS_A                         0171                            120111                          863 @�OY7�UN1   @�OY��^L@<l������c��
=q1   GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                      �A   A   A   @9��@�  @���@���A!��A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}y�D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�|�D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�|�D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�<�D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��fD��f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@4z�@z�H@�=q@�=qA Q�A>�RA^�RA~�RA�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)B�B�B�B�B'�B/�B7�B?�BG�BO�BW�B_�Bg�Bo�Bw�B�B��
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
C�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D z�D ��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��D	z�D	��D
z�D
��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��D z�D ��D!z�D!��D"z�D"��D#z�D#��D$z�D$��D%z�D%��D&z�D&��D'z�D'��D(z�D(��D)z�D)��D*z�D*��D+z�D+��D,z�D,��D-z�D-��D.z�D.��D/z�D/��D0z�D0��D1z�D1��D2z�D2��D3z�D3��D4z�D4��D5z�D5��D6z�D6��D7z�D7��D8z�D8��D9z�D9��D:z�D:��D;z�D;��D<z�D<��D=z�D=��D>z�D>��D?z�D?��D@z�D@��DAz�DA��DBz�DB��DCz�DC��DDz�DD��DEz�DE��DFz�DF��DGz�DG��DHz�DH��DIz�DI��DJz�DJ��DKz�DK��DLz�DL��DMz�DM��DNz�DN��DOz�DO��DPz�DP��DQz�DQ��DRz�DR��DSz�DS��DTz�DT��DUz�DU��DVz�DV��DWz�DW��DXz�DX��DYz�DY��DZz�DZ��D[z�D[��D\z�D\��D]z�D]��D^z�D^��D_z�D_��D`z�D`��Daz�Da��Dbz�Db��Dcz�Dc��Ddz�Dd��Dez�De��Dfz�Df��Dgz�Dg��Dhz�Dh��Diz�Di��Djz�Dj��Dkz�Dk��Dlz�Dl��Dmz�Dm��Dnz�Dn��Doz�Do��Dpz�Dp��Dqz�Dq��Drz�Dr��Dsz�Ds��Dtz�Dt��Duz�Du��Dvz�Dv��Dwz�Dw��Dxz�Dx��Dyz�Dy��Dzz�Dz��D{z�D{��D|z�D|��D}t{D}��D~z�D~��Dz�D��D�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD½qD��qD�=qD�}qDýqD��qD�=qD�}qDĽqD��qD�=qD�}qDŽqD��qD�=qD�}qDƽqD��qD�=qD�}qDǽqD��qD�=qD�}qDȽqD��qD�=qD�}qDɽqD��qD�=qD�}qDʽqD��qD�=qD�}qD˽qD��qD�=qD�}qD̽qD��qD�=qD�}qDͽqD��qD�=qD�}qDνqD��qD�=qD�}qDϽqD��qD�=qD�}qDнqD��qD�=qD�}qDѽqD��qD�=qD�}qDҽqD��qD�=qD�}qDӽqD��qD�=qD�}qDԽqD��qD�=qD�}qDսqD��qD�=qD�}qDֽqD��qD�=qD�}qD׽qD��qD�=qD�}qDؽqD��qD�=qD�}qDٽqD��qD�=qD�}qDڽqD��qD�=qD�}qD۽qD��qD�=qD�}qDܽqD��qD�=qD�}qDݽqD��qD�=qD�}qD޽qD��qD�=qD�}qD߽qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�z>D�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�z>D��qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�:>D�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD���D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A���A���A��A��A���A��A��-A��FA��FA��RA��FA��FA���A���A���A���A���A��uA�~�A�r�A�ZA��A��A�z�A��PA�K�A�G�A�E�A�7LA�VA�1A��A�p�A���A��A��A��7A���A�\)A��uA�A�A�n�A�+A�n�A���A�VA���A���A��7A�(�A�n�A�I�A��^A��A�ȴA���A�?}A���A���A��A�G�A�S�A��RA��A�JA�1'A�\)A��hA�ZA���A�1A��A��HA�ȴA�C�A��A���A��\A��`A�n�A��mA�x�A���A�C�A��mA�bA���A�S�A�bA��DA~��A|�Az�DAvQ�At�Atz�As�Asx�Ar��Aot�AnZAm��Ak�Ah�RAdĜAcƨAb�AaS�A_&�A^v�A]�^A\�A[XAZȴAZ=qAX=qAUS�ASx�AR�AQ�AQ�AP��AP��AP5?AOdZANz�AN�AM�-AMK�AMAL��ALffAKl�AHz�AE+ACG�ABn�AA��AA\)A@1'A>�HA>�+A>�A<z�A;|�A:�DA9�A9��A9&�A8��A8�+A8A�A7ƨA6��A6��A6A4�yA3��A3��A3�A3XA3"�A25?A1�7A1p�A1"�A0~�A0-A/�mA/��A/�7A.�A.~�A.M�A-�#A-l�A,�9A+�
A+;dA*�yA*�+A*bA)��A)`BA)+A(�DA'��A'\)A&^5A%
=A$z�A$ �A"�RA"1'A ffA��A/A  AK�AA�A��A��A�FAXA�A�A��A�A�\A�A�;A�FAdZA/A�9AVA��A��A�hA�A1'A�A"�A�jAffA��AC�A�+A{AS�A=qAA|�AS�A
��A	x�AE�A��A{A��A/A�AI�A�AK�AI�A ��A J@�ȴ@�/@�  @��P@�-@�b@�C�@�ȴ@�`B@�j@��H@�=q@���@�@�x�@�(�@���@�ȴ@�7@�&�@��u@��@���@ٲ-@ׅ@ՙ�@��m@��y@�J@ѩ�@��`@�(�@�\)@�p�@̓u@�  @��@Ų-@�(�@�~�@��7@�l�@���@��@��@�A�@�I�@���@��P@��@�^5@��@�b@��F@�"�@�-@��@���@��@�z�@�b@��
@�dZ@�ȴ@�^5@��@��@�?}@���@�1'@��y@��#@��@��j@�r�@��@���@�;d@��H@�M�@�X@�ƨ@�%@�l�@���@�hs@���@�r�@�b@�t�@���@��h@��@�Ĝ@�Z@�(�@�b@�l�@��y@���@�ff@�M�@�{@��@��#@��@��@�
=@��@��R@���@�E�@���@�7L@���@�Q�@��w@�@��@���@��\@�E�@�J@��T@��7@�?}@�%@��/@���@��@�I�@��;@���@�ȴ@�ff@�-@��@��-@�O�@�r�@�1@��m@���@�+@�V@��#@��-@���@��h@��@�`B@��@��@���@�\)@���@��+@�E�@�-@��@��@�&�@��`@�Ĝ@��@�9X@��m@��F@��P@�33@���@�n�@�J@��-@�x�@�/@��@���@��@�bN@��@
=@~�+@~v�@~v�@~ff@~@}�@}�@}�@|��@|j@{�m@{33@z�@z^5@y��@y�7@y%@x��@x��@xr�@w�;@w\)@vȴ@vV@u��@u�h@uO�@u�@t�j@sƨ@sS�@s33@s"�@r��@r~�@r~�@rn�@r^5@r^5@r^5@q�#@qX@p��@pbN@o�w@oK�@n�y@n��@nV@n{@m�-@m?}@l��@lZ@k�m@kdZ@k@j��@j-@i�^@iX@h�`@hĜ@h�9@hr�@h1'@hb@h  @g�;@g�;@g��@g�P@g\)@g�@f�+@fV@e�T@e�h@e`B@e?}@d�/@d�D@d(�@cC�@b��@b~�@bM�@bJ@a��@a�7@`��@`�u@`Q�@`A�@`b@_�;@_��@_�;@_�;@_�;@_�P@^ȴ@^ff@^E�@]�-@]O�@\�@\�D@\z�@\I�@\1@[��@Z�H@Z��@Z~�@Z=q@Z�@Y��@Y�7@Yhs@YX@YX@Y&�@X��@XĜ@X�u@XQ�@XA�@XA�@W�@W��@W|�@Vȴ@Vȴ@Vȴ@V�R@V�R@Vff@V$�@U��@U�@TZ@S�
@Sƨ@S�
@S��@SdZ@SdZ@R�@R�\@R�\@R^5@R=q@Q�@Q�7@Qx�@Qhs@Qhs@QX@Q%@P��@P�@P�@PbN@O�@Ol�@OK�@N��@N�+@Nv�@NV@N$�@M@M`B@MV@L��@L�j@L��@L�D@Lz�@Lz�@LI�@K��@K�
@K��@K��@K�@KC�@J��@J~�@Jn�@JM�@I�@Ix�@I7L@H��@H�u@HA�@G�@G�w@G|�@G+@G
=@F��@F�@F�R@F��@F��@F�+@F{@E�-@Ep�@E`B@E?}@D�/@DZ@D(�@C�
@CdZ@C@B��@B�@A�#@A��@A�^@A�7@Ahs@A&�@@��@@�@@b@?�@?��@?|�@?K�@>ȴ@>E�@=�-@=�@=�@<�/@<�@<z�@<9X@<�@<1@;ƨ@;�F@;�@;@:�@:�\@:n�@:J@9�^@9hs@9G�@9�@8��@8�`@8Ĝ@8Q�@7�;@7K�@7+@6ȴ@6��@6��@6v�@6$�@5��@5��@5�@5V@4�@4��@4Z@41@3��@3t�@333@3o@2�H@2^5@2J@1�#@1G�@1%@0��@0Ĝ@0��@0��@0�@0A�@0b@0  @/��@/�w@/�@/K�@/+@.�@.ff@.5?@.$�@.@-��@-��@-�h@-�@-`B@-�@,��@,9X@+�
@+��@+��@+t�@+dZ@+33@+"�@+o@*�@*��@*��@*��@*��@*�\@*n�@*-@)�^@)��@)��@)hs@)X@)7L@)7L@)G�@)7L@)&�@(��@(bN@'�@'��@'K�@&�y@&��@&E�@&5?@&@%@%/@$��@$�/@$��@$�@$�@$��@$�D@$z�@$j@#��@#�@#t�@"��@"-@!��@!��@!G�@!�@!�@!%@ �`@ �9@ r�@ r�@  �@  �@  �@ b@   @�@|�@;d@�@�@
=@��@�y@�R@��@��@�+@v�@ff@E�@@@`B@O�@/@/@/@V@�j@j@�@1@1@ƨ@t�@S�@S�@S�@S�@S�@o@�H@�@�7@X@7L@&�@&�@�@%@��@Ĝ@�u@bN@1'@�@|�@�y@ȴ@��@��@��@{@@�@�T@��@V@�j@�D@j@9X@1@1@�
@ƨ@ƨ@ƨ@�F@S�@C�@33@"�@"�@o@o@o@o@o@@��@~�@M�@�#@�7@hs@X@7L@�9@1'@�;@�@�P@l�@l�@\)@K�@;d@
=@�@ȴ@��@��@��@�+@V@5?@@@�@/@�@V@V@��@��@�/@��@z�@9X@�@�
@�
@�
@�@@
�H@
��@
�!@
�\@
~�@
n�@
^5@
=q@
�@	�#@	��@	��@	x�@	G�@	&�@	%@��@�u@�@Q�@1'@b@  @  @�;@�@l�@K�@
=@
=@�y@�@ȴ@��@��@��@��@�+@ff@5?@{@�@�-@�-@��@�@p�@p�@`B@O�@?}@?}@?}@/@�@�@V@��@��@�@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A���A���A���A��A��A���A��A��-A��FA��FA��RA��FA��FA���A���A���A���A���A��uA�~�A�r�A�ZA��A��A�z�A��PA�K�A�G�A�E�A�7LA�VA�1A��A�p�A���A��A��A��7A���A�\)A��uA�A�A�n�A�+A�n�A���A�VA���A���A��7A�(�A�n�A�I�A��^A��A�ȴA���A�?}A���A���A��A�G�A�S�A��RA��A�JA�1'A�\)A��hA�ZA���A�1A��A��HA�ȴA�C�A��A���A��\A��`A�n�A��mA�x�A���A�C�A��mA�bA���A�S�A�bA��DA~��A|�Az�DAvQ�At�Atz�As�Asx�Ar��Aot�AnZAm��Ak�Ah�RAdĜAcƨAb�AaS�A_&�A^v�A]�^A\�A[XAZȴAZ=qAX=qAUS�ASx�AR�AQ�AQ�AP��AP��AP5?AOdZANz�AN�AM�-AMK�AMAL��ALffAKl�AHz�AE+ACG�ABn�AA��AA\)A@1'A>�HA>�+A>�A<z�A;|�A:�DA9�A9��A9&�A8��A8�+A8A�A7ƨA6��A6��A6A4�yA3��A3��A3�A3XA3"�A25?A1�7A1p�A1"�A0~�A0-A/�mA/��A/�7A.�A.~�A.M�A-�#A-l�A,�9A+�
A+;dA*�yA*�+A*bA)��A)`BA)+A(�DA'��A'\)A&^5A%
=A$z�A$ �A"�RA"1'A ffA��A/A  AK�AA�A��A��A�FAXA�A�A��A�A�\A�A�;A�FAdZA/A�9AVA��A��A�hA�A1'A�A"�A�jAffA��AC�A�+A{AS�A=qAA|�AS�A
��A	x�AE�A��A{A��A/A�AI�A�AK�AI�A ��A J@�ȴ@�/@�  @��P@�-@�b@�C�@�ȴ@�`B@�j@��H@�=q@���@�@�x�@�(�@���@�ȴ@�7@�&�@��u@��@���@ٲ-@ׅ@ՙ�@��m@��y@�J@ѩ�@��`@�(�@�\)@�p�@̓u@�  @��@Ų-@�(�@�~�@��7@�l�@���@��@��@�A�@�I�@���@��P@��@�^5@��@�b@��F@�"�@�-@��@���@��@�z�@�b@��
@�dZ@�ȴ@�^5@��@��@�?}@���@�1'@��y@��#@��@��j@�r�@��@���@�;d@��H@�M�@�X@�ƨ@�%@�l�@���@�hs@���@�r�@�b@�t�@���@��h@��@�Ĝ@�Z@�(�@�b@�l�@��y@���@�ff@�M�@�{@��@��#@��@��@�
=@��@��R@���@�E�@���@�7L@���@�Q�@��w@�@��@���@��\@�E�@�J@��T@��7@�?}@�%@��/@���@��@�I�@��;@���@�ȴ@�ff@�-@��@��-@�O�@�r�@�1@��m@���@�+@�V@��#@��-@���@��h@��@�`B@��@��@���@�\)@���@��+@�E�@�-@��@��@�&�@��`@�Ĝ@��@�9X@��m@��F@��P@�33@���@�n�@�J@��-@�x�@�/@��@���@��@�bN@��@
=@~�+@~v�@~v�@~ff@~@}�@}�@}�@|��@|j@{�m@{33@z�@z^5@y��@y�7@y%@x��@x��@xr�@w�;@w\)@vȴ@vV@u��@u�h@uO�@u�@t�j@sƨ@sS�@s33@s"�@r��@r~�@r~�@rn�@r^5@r^5@r^5@q�#@qX@p��@pbN@o�w@oK�@n�y@n��@nV@n{@m�-@m?}@l��@lZ@k�m@kdZ@k@j��@j-@i�^@iX@h�`@hĜ@h�9@hr�@h1'@hb@h  @g�;@g�;@g��@g�P@g\)@g�@f�+@fV@e�T@e�h@e`B@e?}@d�/@d�D@d(�@cC�@b��@b~�@bM�@bJ@a��@a�7@`��@`�u@`Q�@`A�@`b@_�;@_��@_�;@_�;@_�;@_�P@^ȴ@^ff@^E�@]�-@]O�@\�@\�D@\z�@\I�@\1@[��@Z�H@Z��@Z~�@Z=q@Z�@Y��@Y�7@Yhs@YX@YX@Y&�@X��@XĜ@X�u@XQ�@XA�@XA�@W�@W��@W|�@Vȴ@Vȴ@Vȴ@V�R@V�R@Vff@V$�@U��@U�@TZ@S�
@Sƨ@S�
@S��@SdZ@SdZ@R�@R�\@R�\@R^5@R=q@Q�@Q�7@Qx�@Qhs@Qhs@QX@Q%@P��@P�@P�@PbN@O�@Ol�@OK�@N��@N�+@Nv�@NV@N$�@M@M`B@MV@L��@L�j@L��@L�D@Lz�@Lz�@LI�@K��@K�
@K��@K��@K�@KC�@J��@J~�@Jn�@JM�@I�@Ix�@I7L@H��@H�u@HA�@G�@G�w@G|�@G+@G
=@F��@F�@F�R@F��@F��@F�+@F{@E�-@Ep�@E`B@E?}@D�/@DZ@D(�@C�
@CdZ@C@B��@B�@A�#@A��@A�^@A�7@Ahs@A&�@@��@@�@@b@?�@?��@?|�@?K�@>ȴ@>E�@=�-@=�@=�@<�/@<�@<z�@<9X@<�@<1@;ƨ@;�F@;�@;@:�@:�\@:n�@:J@9�^@9hs@9G�@9�@8��@8�`@8Ĝ@8Q�@7�;@7K�@7+@6ȴ@6��@6��@6v�@6$�@5��@5��@5�@5V@4�@4��@4Z@41@3��@3t�@333@3o@2�H@2^5@2J@1�#@1G�@1%@0��@0Ĝ@0��@0��@0�@0A�@0b@0  @/��@/�w@/�@/K�@/+@.�@.ff@.5?@.$�@.@-��@-��@-�h@-�@-`B@-�@,��@,9X@+�
@+��@+��@+t�@+dZ@+33@+"�@+o@*�@*��@*��@*��@*��@*�\@*n�@*-@)�^@)��@)��@)hs@)X@)7L@)7L@)G�@)7L@)&�@(��@(bN@'�@'��@'K�@&�y@&��@&E�@&5?@&@%@%/@$��@$�/@$��@$�@$�@$��@$�D@$z�@$j@#��@#�@#t�@"��@"-@!��@!��@!G�@!�@!�@!%@ �`@ �9@ r�@ r�@  �@  �@  �@ b@   @�@|�@;d@�@�@
=@��@�y@�R@��@��@�+@v�@ff@E�@@@`B@O�@/@/@/@V@�j@j@�@1@1@ƨ@t�@S�@S�@S�@S�@S�@o@�H@�@�7@X@7L@&�@&�@�@%@��@Ĝ@�u@bN@1'@�@|�@�y@ȴ@��@��@��@{@@�@�T@��@V@�j@�D@j@9X@1@1@�
@ƨ@ƨ@ƨ@�F@S�@C�@33@"�@"�@o@o@o@o@o@@��@~�@M�@�#@�7@hs@X@7L@�9@1'@�;@�@�P@l�@l�@\)@K�@;d@
=@�@ȴ@��@��@��@�+@V@5?@@@�@/@�@V@V@��@��@�/@��@z�@9X@�@�
@�
@�
@�@@
�H@
��@
�!@
�\@
~�@
n�@
^5@
=q@
�@	�#@	��@	��@	x�@	G�@	&�@	%@��@�u@�@Q�@1'@b@  @  @�;@�@l�@K�@
=@
=@�y@�@ȴ@��@��@��@��@�+@ff@5?@{@�@�-@�-@��@�@p�@p�@`B@O�@?}@?}@?}@/@�@�@V@��@��@�@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�BB�BB�;B�;B�;B�;B�;B�;B�;B�;B�;B�;B�;B�;B�;B�;B�;B�;B�;B�;B�;B�;B�BB�TB��BDBPBPBPBJBDBDB	7BB��B��B��B�B�#B��B�B�BP�B9XB�BB��B��B�B�RB��B�VB�DB�B� B� B|�Bu�Bl�BhsBdZB[#BO�BF�BYBaHBhsBo�By�Bz�Bp�BaHBS�BJ�BH�B=qB&�B$�B�B{BDBB
�B
�5B
�
B
��B
��B
�dB
�LB
�3B
�B
�\B
� B
m�B
S�B
J�B
G�B
C�B
>wB
5?B
"�B
�B
hB	��B	�ZB	��B	ŢB	�wB	�?B	��B	��B	��B	��B	��B	��B	�oB	}�B	n�B	r�B	o�B	l�B	iyB	ffB	e`B	bNB	_;B	[#B	YB	W
B	T�B	R�B	P�B	L�B	E�B	7LB	(�B	 �B	�B	�B	uB	DB	%B	B	B��B�B�B�sB�fB�`B�TB�NB�HB�;B�#B�B��B��B��B��B��B��B��BȴBŢBĜBÖB��B�}B�wB�qB�qB�^B�RB�LB�FB�9B�'B�B�B�B��B��B��B��B��B��B��B��B��B��B��B�oB�VB�DB�+B�B�B� B}�B{�Bz�Bz�By�By�Bw�Bs�Bo�Bl�Bk�BjBiyBhsBgmBffBe`BdZBcTBbNBaHB`BB]/B\)B[#BYBXBVBT�BR�BP�BN�BK�BJ�BI�BG�BE�BB�B>wB:^B8RB6FB5?B49B33B2-B0!B-B)�B(�B&�B%�B#�B"�B �B�B�B�B�B�B�B�BuBoBhBbBVBPBPBJBDB	7B1B+B+B+B+B+B+B+B+B%B%B+B+B%BB1B	7B
=B
=BPBVBbBhBoB�B �B%�B&�B&�B&�B)�B,B-B-B,B-B.B0!B5?B6FB7LB8RB9XB:^B;dB;dB;dB<jB?}BB�BD�BE�BF�BH�BH�BI�BI�BI�BH�BD�BC�BE�BG�BI�BM�BO�BQ�BS�BVBXBYBYBYBYBXBYB[#B]/B^5B_;B_;B_;BaHBdZBgmBm�Bq�Br�Bs�Bs�Bt�Bu�Bv�Bw�By�B}�B~�B~�B�B�B�B�B�%B�+B�7B�=B�=B�=B�JB�\B�\B��B��B��B��B��B��B��B��B��B��B�B�'B�?B�FB�LB�LB�RB�RB�jB�wBƨBǮB��B��B��B��B��B��B�B�)B�/B�;B�HB�ZB�`B�fB�yB�B�B�B��B��B��B��B��B��B	  B	B	1B	
=B	DB	DB	DB	PB	PB	\B	bB	oB	{B	�B	�B	�B	�B	 �B	!�B	$�B	%�B	%�B	&�B	)�B	,B	.B	0!B	33B	49B	5?B	6FB	8RB	>wB	@�B	@�B	A�B	C�B	D�B	D�B	E�B	E�B	F�B	F�B	J�B	L�B	M�B	O�B	Q�B	S�B	VB	XB	YB	ZB	]/B	`BB	bNB	dZB	ffB	hsB	jB	k�B	m�B	o�B	q�B	r�B	s�B	s�B	t�B	u�B	v�B	v�B	w�B	w�B	w�B	x�B	y�B	z�B	}�B	}�B	�B	�B	�B	�B	�%B	�+B	�7B	�PB	�\B	�hB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�'B	�?B	�FB	�FB	�LB	�RB	�^B	�^B	�dB	�dB	�dB	�dB	�jB	�qB	�wB	�}B	�}B	�}B	��B	��B	B	ĜB	ĜB	ĜB	ŢB	ŢB	ƨB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�#B	�)B	�)B	�)B	�BB	�HB	�HB	�TB	�ZB	�ZB	�`B	�fB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
%B
+B
+B
1B
	7B

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
\B
\B
bB
hB
uB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
!�B
"�B
#�B
#�B
#�B
$�B
%�B
%�B
%�B
&�B
'�B
'�B
(�B
(�B
(�B
)�B
+B
+B
,B
-B
-B
-B
-B
-B
.B
.B
/B
/B
/B
/B
/B
0!B
0!B
0!B
1'B
2-B
2-B
2-B
2-B
33B
33B
33B
33B
33B
49B
5?B
6FB
6FB
6FB
6FB
7LB
7LB
7LB
7LB
7LB
8RB
8RB
8RB
8RB
8RB
8RB
9XB
:^B
:^B
:^B
:^B
;dB
;dB
;dB
;dB
;dB
;dB
;dB
<jB
=qB
=qB
>wB
?}B
?}B
@�B
@�B
@�B
@�B
A�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
C�B
D�B
D�B
E�B
F�B
F�B
F�B
G�B
G�B
H�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
L�B
L�B
M�B
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
P�B
P�B
P�B
P�B
P�B
Q�B
R�B
R�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
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
[#B
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
\)B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
]/B
]/B
]/B
^5B
_;B
_;B
_;B
_;B
`BB
aHB
aHB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
dZB
dZB
dZB
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
gmB
gmB
gmB
gmB
gmB
gmB
hsB
iyB
iyB
iyB
iyB
iyB
iyB
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
k�B
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
n�B
n�B
n�B
n�B
n�B
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
q�B
q�B
q�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�DB�AB�.B�CB�IB�>B�3B�8B�CB�<B�IB�CB�B�CB�CB�mB�gB�rB߰B߉B��B�B��B��BB�BkBcB�B�BdB�B
�B�B�$B��B�ZB�0B�3B��B�oB�<BVPB@�B�B2B�WB��B��B��B�B�)B�B�3B��B��B~�BxBm�Bk>Bi)B`ABXnBE�BXaB`{Bg�Bn�B{*B}�Bt�Bc�BV�BK,BJ�BA�B'uB&�B"mB^B�B�B
�B
��B
ؠB
�^B
�@B
��B
�tB
�xB
�9B
��B
�lB
v�B
WB
K�B
H�B
D�B
@]B
<<B
%HB
RB
�B
B	�B	�	B	�B	��B	��B	��B	��B	�kB	��B	��B	��B	�
B	��B	r�B	uB	qB	m�B	k7B	f�B	f�B	dyB	a�B	\8B	Z-B	X&B	U�B	S�B	Q�B	O�B	M�B	A!B	.�B	#SB	B	�B	�B	�B	HB	�B	�B��B�nB�pB�B��B��B�B�*B�B�B�
B�WB�QB�SB�jB�B�AB�UBͺB�4B��B�XB�B�AB�$B�!B��B��B�^B��B�WB�KB�B�2B��B��B�B�BB�B��B��B��B�ZB�yB�iB�MB�$B��B�,B�B�:B�ZB�eB�BB��B��B|�B{]B{6Bz�B{8BzJBwBswBm�Bl�Bk!Bi�BiGBh
Bg�BfTBe=Bc�Bb�Bb�Bb�B^�B](B\!BY�BY�BWbBV�BT!BR�BQvBMBKuBJ1BI�BHBE�BB�B;�B9vB7�B6�B59B41B3�B2�B0�B,B*�B(�B'wB$�B$�B#�B �BrB�BBB"B�BBBbB-B�B�B�B�B�BB�B
�B	�B	�B	4BcBBB�BB(BRB�BFBB�B
�B
&BfB�B�B\BUB�BBrBB!eB&�B'�B(�B(TB*�B,�B._B-~B-�B-xB.,B0�B5�B6�B84B8�B9�B;B;�B;�B<ZB>#B@�BC�BE/BFBGeBI'BIDBJFBJ�BK BJ�BH�BE�BF�BH�BJ�BNBPTBR�BT�BWTBX�BY�BY�BY^BYABX�BY�B[�B]xB^aB_�B_}B_dBb�Be_Bh�Bm�Bq�Br�Bt7BtiBu�Bv�BwOBx�Bz�B~8BBfB��B�gB�aB��B��B��B�|B�eB�}B��B��B��B��B�B��B��B�B�SB��B�~B�)B�gB��B�GB��B��B�tB�cB�qB��B��B�B��B�B�CB�kB�;B�B�B�EB�BڃB�fBݕB߬B��B�B�B��B�$B�B�/B�9B�B�:B�=B�#B�&B�fB	 �B	�B	�B	
VB	OB	\B	�B	iB	�B	�B	�B	�B	�B	B	�B	B	*B	!B	"7B	%B	&B	&B	'^B	*^B	,kB	.aB	0vB	3eB	4nB	5gB	6�B	8�B	>�B	@�B	@�B	A�B	C�B	D�B	D�B	E�B	E�B	F�B	G
B	K"B	MB	NIB	P^B	RGB	TFB	VHB	XJB	YTB	ZnB	]�B	`�B	b�B	d�B	f�B	h�B	j�B	k�B	m�B	o�B	rB	r�B	s�B	s�B	t�B	u�B	v�B	v�B	w�B	w�B	xB	yB	zB	{`B	~*B	~[B	�NB	�=B	�8B	�qB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�gB	�7B	�B	�eB	�MB	�XB	�]B	�&B	�CB	�TB	�qB	��B	�qB	�cB	�yB	�jB	��B	�|B	�|B	�|B	�nB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	ĥB	ħB	ĳB	ŮB	��B	��B	�B	�B	�aB	�1B	��B	��B	�B	�
B	��B	�AB	�;B	�B	�$B	�B	�CB	�PB	�%B	�'B	�B	�'B	�YB	�`B	�BB	�3B	�KB	ܱB	�vB	�lB	�B	�B	�qB	�zB	�B	�B	��B	�B	�B	�B	�B	��B	��B	�B	��B	��B	��B	��B	�B	�B	��B	�B	��B	��B	��B	�B	�B	�B	�	B	� B	�B	�!B	�B	� B	�4B	�B	�B	�B	�B	�B	�B	�B	�VB
_B
?B
#B
4B
`B
B
SB
iB
�B
{B
lB
�B
	sB

SB

VB

kB
gB
�B
}B
�B
�B
zB
�B
xB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
B
 (B
 �B
!B
!�B
!�B
!�B
"B
#B
$B
#�B
$.B
%(B
%�B
&B
&(B
'7B
(B
()B
)B
)&B
)^B
*>B
+1B
+qB
,BB
-)B
-<B
--B
-B
-3B
.LB
.@B
/.B
/GB
/3B
/3B
/jB
0HB
0jB
0|B
1TB
2FB
2MB
2ZB
2[B
3KB
3JB
3VB
3oB
3�B
4�B
5�B
6vB
6_B
6jB
6aB
7xB
7bB
7`B
7nB
7lB
8]B
8[B
8kB
8�B
8qB
8�B
9�B
:tB
:uB
:�B
:tB
;�B
;nB
;cB
;zB
;|B
;�B
;�B
<�B
=�B
=�B
>�B
?�B
?�B
@�B
@�B
@�B
@�B
A�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
C�B
D�B
ENB
FB
F�B
F�B
GB
G�B
G�B
H�B
H�B
H�B
H�B
H�B
I B
I�B
I�B
I�B
I�B
I�B
J B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
M B
MB
M�B
M�B
M�B
M�B
M�B
NB
OB
OB
O�B
O�B
PB
PB
QB
P�B
P�B
P�B
P�B
QB
QB
QyB
R[B
S!B
SB
TB
TB
TB
TB
TB
T+B
T'B
T%B
U/B
U9B
U`B
V{B
W0B
W-B
X(B
X!B
XB
X)B
Y1B
Y1B
YYB
Y�B
ZhB
ZJB
ZBB
ZMB
[RB
[0B
[TB
[:B
[-B
[0B
[@B
[{B
\BB
\BB
\BB
\6B
\AB
\4B
\5B
\5B
\6B
\FB
\cB
]}B
]gB
]�B
^�B
_aB
_TB
_hB
_�B
`�B
a�B
azB
btB
bsB
b\B
biB
bgB
blB
b�B
c�B
cmB
cyB
cjB
c^B
cnB
c�B
d�B
d�B
d�B
d�B
e�B
ewB
ewB
fsB
fB
fsB
f�B
fB
f�B
f�B
g�B
g�B
gxB
g}B
g�B
g�B
h�B
i�B
i�B
i�B
i�B
i�B
i�B
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
k�B
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
n�B
n�B
n�B
n�B
n�B
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
q�B
q�B
q�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<+K�<S�3<#�
<#�
<#�
<#�
<#�
<#�
<8�?<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<(��<#�
<#�
<'�2<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<1]E<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - (REPORTED_SURFACE_PRESSURE) for PADJ                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al,2007,JAOT & effects of pressure adjustments                                                                                                                                                            PADJ REPORTED_SURFACE_PRESSURE =0.08 dbar                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CTM alpha = 0.141 & tau = 6.68 s with error equal to the correction                                                                                                                                                                                             Pressures adjusted using reported surface pressure when warranted.                                                                                                                                                                                              none                                                                                                                                                                                                                                                            Salinity corrected for pressure sensor calibration drift and cell thermal mass                                                                                                                                                                                  201810180929262018101809292620181018092926  AO  ARCAADJP                                                                    20180226180253    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20180226180253  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20180226180253  QCF$                G�O�G�O�G�O�0               PM  ARSQPADJV1.1                                                                20181018092926  QC  PRES            @9��D��fG�O�                PM  ARSQCTM V1.1                                                                20181018092926  QC  PSAL            @9��D��fG�O�                PM  ARSQOWGUV1.0CTD_2017v1 + Argo_2017v02                                       20181023151222  IP                  G�O�G�O�G�O�                