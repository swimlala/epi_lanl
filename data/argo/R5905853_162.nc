CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2023-07-18T18:43:56Z creation;2023-07-18T18:43:57Z conversion to V3.1      
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
_FillValue                 �  I4   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M$   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p|   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tl   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �h   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �X   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �t   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ޤ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �    HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �0   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �4   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �D   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �H   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �L   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20230718184356  20230718185725  5905853                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA                                  2B  A   APEX                            8421                            2.11.2                          846 @�;g""""1   @�;g��l�@1kI�^�cx     1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @9��@�  @�  A   A   A@  A^ffA�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  BhffBpffBx  B�  B�33B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�ffBǙ�B���B�  B�  B�  B�  B�  B�  B�  B�33B�33B�ffB�33B���B���C�fC  C  C  C
  C  C  C  C  C  C�C33C�fC  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB33CC��CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Ck�fCn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fDfDy�D��D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD�fDE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DMfDM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]�fD^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do�fDp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր Dּ�D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D��3D�3D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�9�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @5�@{�@�@�A�HA>�HA]G�A~�HA�p�A�p�A�p�A�p�A�p�A�p�A�p�A�p�B�RB�RB�RB�RB'�RB/�RB7�RB?�RBG�RBO�RBW�RB_�RBh�Bp�Bw�RB�RB�\B��)B��)B��)B��)B���B��)B��)B��)B��)B��)B��)B��)B�\B�\B��)B�B�B�u�B˨�B��)B��)B��)B��)B��)B��)B��)B�\B�\B�B�B�\B���B���C�zC�C�C�C	�C�C�C�C�C�C�C!GC�zC�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CB!GCC��CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�zCm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��C��C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��=C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
D {�D ��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D	{�D	��D
{�D
��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D��D�DuD�D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D {�D ��D!{�D!��D"{�D"��D#{�D#��D${�D$��D%{�D%��D&{�D&��D'{�D'��D({�D(��D){�D)��D*{�D*��D+{�D+��D,{�D,��D-{�D-��D.{�D.��D/{�D/��D0{�D0��D1{�D1��D2{�D2��D3{�D3��D4{�D4��D5{�D5��D6{�D6��D7{�D7��D8{�D8��D9{�D9��D:{�D:��D;{�D;��D<{�D<��D={�D=��D>{�D>��D?{�D?��D@{�D@��DA{�DA��DB{�DB��DC{�DC��DD��DD��DE{�DE��DF{�DF��DG{�DG��DH{�DH��DI{�DI��DJ{�DJ��DK{�DK��DL{�DM�DM{�DM��DN{�DN��DO{�DO��DP{�DP��DQ{�DQ��DR{�DR��DS{�DS��DT{�DT��DU{�DU��DV{�DV��DW{�DW��DX{�DX��DY{�DY��DZ{�DZ��D[{�D[��D\{�D\��D]��D]��D^{�D^��D_{�D_��D`{�D`��Da{�Da��Db{�Db��Dc{�Dc��Dd{�Dd��De{�De��Df{�Df��Dg{�Dg��Dh{�Dh��Di{�Di��Dj{�Dj��Dk{�Dk��Dl{�Dl��Dm{�Dm��Dn{�Dn��Do��Do��Dp{�Dp��Dq{�Dq��Dr{�Dr��Ds{�Ds��Dt{�Dt��Du{�Du��Dv{�Dv��Dw{�Dw��Dx{�Dx��Dy{�Dy��Dz{�Dz��D{{�D{��D|{�D|��D}{�D}��D~{�D~��D{�D��D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D½�D���D�=�D�}�Dý�D���D�=�D�}�DĽ�D���D�=�D�}�DŽ�D���D�=�D�}�Dƽ�D���D�=�D�}�Dǽ�D���D�=�D�}�DȽ�D���D�=�D�}�Dɽ�D���D�=�D�}�Dʽ�D���D�=�D�}�D˽�D���D�=�D�}�D̽�D���D�=�D�}�Dͽ�D���D�=�D�}�Dν�D���D�=�D�}�DϽ�D���D�=�D�}�Dн�D���D�=�D�}�Dѽ�D���D�=�D�}�Dҽ�D���D�=�D�}�Dӽ�D���D�=�D�}�DԽ�D���D�=�D�}�Dս�D���D�=�D�}�Dֺ�D���D�=�D�}�D׽�D���D�=�D�}�Dؽ�D���D�=�D�}�D���D� �D�=�D�}�Dڽ�D���D�=�D�}�D۽�D���D�=�D�}�Dܽ�D���D�=�D�}�Dݽ�D���D�=�D�}�D޽�D���D�=�D�}�D߽�D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D���D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�7]111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A� A�A��A��A� 'A� 'A�!bA� �A��A� �A�"�A�$@A�%FA�"�A�#:A�&LA�&�A�$A�~A� \A�hA��A��A�=A��A��A��A��A�xA�xA� A� A��AήIA� 'A��A͊=A�aA�tA�F?A�"�AƆYA�(�A���A�oA��A�ҽA�=<A�uA���A�o�A���A�G�A�+�A�u�A�V�A��-A���A�ŢA��
A��UA�E�A�,A�3�A��RA�A�A�'�A�j�A�U�A�4�A�/�A��'A�5A��A���A���A���A�!bA�҉A�$A�T�A�QNA�_A�`�A��cA���A�g�A��_A��IA�=<A�zxA���A~�DAz�)Ay�Ax	At�HAn�cAh}VAg;�Af�<Af*0Ae��Ad��Aaw2A`&A^y�A[�2AX�AU&�AR�AMI�AJ�AH��AEɆAC��AB�sA?*�A<J�A9�A9 iA7��A6%FA1��A+^5A+�'A,l�A+�8A*&A){�A*qA*�A)��A'�3A%�UA%	lA#�A!�A"[�A#�A#>BA#S�A#�:A#l"A"�AA"�gA!��AXA�A�AA�A�sAxA��A��A�A��A0UA��AL�AA?AX�Ah�A�A�A�KA3�A��AMA@�Al"A��A��A
 iA	�~A�A��AخA�1A��A��A@AC�A��A`BA�OA�0AC�A�.A��AZA/A	A�A	AYAU�A�A�9A�A�9A��Aw�AG�A�}AGEA!�A'�A��AF�ATaA9�AVmA�A oi@�'�@�;d@���@��A �A J@���@�!�@�`�@���@���@��T@�E9@��P@�x@��@�V@��6@��@�j@�z�@쭬@�]�@�7@�Ĝ@�I@��@��@�M@�O@�h�@淀@�b@��K@��@䀝@�7�@��@�@�	�@��@��]@���@�S&@�Q�@߅@�S�@ݕ�@�'�@��U@�y>@��D@�<6@�E9@��/@��6@�D�@�ƨ@ۓ�@�ȴ@ڎ�@�|�@ٜ�@�F�@ؠ�@�@�9�@���@��N@� \@Ԟ@��W@�J�@�}�@�֡@с@σ{@ͣn@��@̳h@�'R@�[W@ʢ4@�Ft@��D@�f�@�bN@��@��@�;d@�Ĝ@ħ�@ĭ�@�m�@öF@�}�@�O�@�*0@�֡@I@�PH@�5?@�&�@��.@��)@���@�qv@�8@��@��@��@���@�<�@�ݘ@��-@�[W@��4@�7@���@�;d@���@���@��K@��s@�[�@�1'@�T�@���@�k�@���@���@���@���@�($@��@�;�@���@�\�@��6@���@���@���@��1@�p;@�2�@�p�@�&@���@���@��)@��@�Ta@�!�@���@���@��	@���@��@�\)@��@�{�@�u@���@�*0@��8@��x@�?@���@���@�N<@��	@��F@��;@��T@�خ@���@��n@�zx@�@��@���@�)�@��@�ϫ@���@��V@��t@���@��'@�n/@��@�y>@�b@���@��~@�A�@���@�@���@�9�@��"@��z@�u%@�R�@�2�@��@��@���@�y�@�6z@�@��@�]d@�b@���@�Mj@�֡@�C�@��o@��'@���@�O@�!�@�%@��[@�� @���@��7@��4@�O@�#�@��@���@���@�0U@��m@���@�>�@�u�@�@��@�w2@��@��z@�~�@�y>@�=q@�o�@��@���@��c@���@���@�ں@�4n@���@���@�iD@��@��@�e�@�PH@�<�@��@���@��a@��C@��V@��$@�~�@�E9@��@���@���@���@�q@�V�@�M@�B[@��D@��H@���@�:�@��"@��p@��9@���@�A�@�(�@��&@��@��@�g�@� \@� i@��@�ߤ@���@���@��4@��u@���@�a|@�0U@�@���@��@�qv@�%@��@���@���@��@���@�W�@���@���@��~@�Z�@�B�@�=@��@��]@��O@�@�� @���@�b�@��@��z@���@��1@���@�/�@���@�� @��n@�/@�	l@�%@��s@���@�r�@�YK@��@���@��@�e�@��@���@��@��2@�xl@�(�@��@���@�c@�b�@�Z�@�@��@�_�@�K^@��@��@W?@,�@~�,@~��@}c�@|�_@|Z@|�@{��@{~�@{�@{�@z�@z@�@y�@y�@xj@x	�@w��@v��@u�@u��@u<6@tg8@t~@s�0@s8@sS@r�F@r�@q��@qhs@q�@p��@p�@o�;@o��@n�@n��@nxl@n5?@m��@mzx@m�@l�@lZ@lI�@l�@k�*@k�@j� @i�@i�X@ic@iO�@h�5@h��@hq@hI�@f�]@f��@f:*@f:*@f#:@f@f_@e�z@e��@eo @e�@d֡@d��@dy>@c�	@b�@b�X@b4@ae,@ac�@a?}@a/@a�@`ѷ@`PH@_�{@^��@^@\�`@\tT@\�@[�Q@[�@[~�@[@O@Z�c@Z�@Zq�@Y��@X[�@W�;@W��@WU�@W��@W��@W��@WC@V=q@V�@U�@U�H@U�~@U5�@T��@T��@T/�@Ss@RW�@Q��@Q��@Qk�@P�@P|�@Pm�@P*�@Og�@N��@Nu%@Nff@Na|@N\�@NR�@NOv@NC�@N8�@Ne@MX@L4n@K�@K�@KX�@J�,@J��@J��@J�A@Ju%@Jn�@JL0@I�o@I�7@If�@IO�@IN<@I@H��@H[�@H1'@G��@F��@F�r@FTa@FGE@F-@F	@F{@F�@FJ@FJ@F	@F �@E�@E�X@D�@D/�@C�	@B��@B+k@A��@As�@A-w@@�@@oi@?��@>��@>@�@=�9@=hs@<�5@<U2@<4n@</�@<(�@<@<@<  @<�@;��@;��@;e�@:͟@:��@:i�@:kQ@:Z�@:#:@9��@9�@9�@8ѷ@8�Y@8~@7�K@7�@7��@7�@6ȴ@6z@6�@4�P@4�@4��@3��@3v`@3v`@3s@3Y@2͟@2�x@2H�@1�@1w2@1Y�@1G�@12a@1�@0��@/��@/��@/��@/��@/�F@/��@/�*@.��@.:*@-�d@-*0@,�4@,_@,<�@+�@+��@+"�@+o@*��@*��@*c @*($@*�@)�>@)��@)��@)e,@(�v@(Ĝ@(�@(�9@(��@(��@(Q�@(  @'� @'�a@'�@'�[@'��@'�V@'�k@'��@'��@'��@'o�@'A�@'!-@'@&�8@&ں@&�@%�M@%7L@% \@$�[@$�@$7@#�
@#�a@#��@#Mj@"�@"xl@"Ov@"-@"#:@"_@!�@!�@!��@!��@!�@!ԕ@!��@!�9@!ϫ@!��@ �@ ��@ tT@ g8@ _@ C-@ -�@�@�[@��@l�@4�@"�@�@�X@��@�\@xl@e@�X@[W@J�@Dg@/@�@�@�@��@C-@!@�@��@��@�@a|@�@#�@�@�@�E@�@~@�M@u%@@�@&�@	@_@��@��@��@�S@o @8�@�@�E@�$@��@�z@��@oi@e�@]d@2�@x@��@��@�k@n/@dZ@g�@a@W?@9�@&@�@҉@B[@ �@��@�@��@�=@Q�@(�@q@��@��@�.@�@j@M@7@�@�@��@خ@�6@��@�$@�	@C�@�@�@�@��@�2@�h@l�@\�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A� A�A��A��A� 'A� 'A�!bA� �A��A� �A�"�A�$@A�%FA�"�A�#:A�&LA�&�A�$A�~A� \A�hA��A��A�=A��A��A��A��A�xA�xA� A� A��AήIA� 'A��A͊=A�aA�tA�F?A�"�AƆYA�(�A���A�oA��A�ҽA�=<A�uA���A�o�A���A�G�A�+�A�u�A�V�A��-A���A�ŢA��
A��UA�E�A�,A�3�A��RA�A�A�'�A�j�A�U�A�4�A�/�A��'A�5A��A���A���A���A�!bA�҉A�$A�T�A�QNA�_A�`�A��cA���A�g�A��_A��IA�=<A�zxA���A~�DAz�)Ay�Ax	At�HAn�cAh}VAg;�Af�<Af*0Ae��Ad��Aaw2A`&A^y�A[�2AX�AU&�AR�AMI�AJ�AH��AEɆAC��AB�sA?*�A<J�A9�A9 iA7��A6%FA1��A+^5A+�'A,l�A+�8A*&A){�A*qA*�A)��A'�3A%�UA%	lA#�A!�A"[�A#�A#>BA#S�A#�:A#l"A"�AA"�gA!��AXA�A�AA�A�sAxA��A��A�A��A0UA��AL�AA?AX�Ah�A�A�A�KA3�A��AMA@�Al"A��A��A
 iA	�~A�A��AخA�1A��A��A@AC�A��A`BA�OA�0AC�A�.A��AZA/A	A�A	AYAU�A�A�9A�A�9A��Aw�AG�A�}AGEA!�A'�A��AF�ATaA9�AVmA�A oi@�'�@�;d@���@��A �A J@���@�!�@�`�@���@���@��T@�E9@��P@�x@��@�V@��6@��@�j@�z�@쭬@�]�@�7@�Ĝ@�I@��@��@�M@�O@�h�@淀@�b@��K@��@䀝@�7�@��@�@�	�@��@��]@���@�S&@�Q�@߅@�S�@ݕ�@�'�@��U@�y>@��D@�<6@�E9@��/@��6@�D�@�ƨ@ۓ�@�ȴ@ڎ�@�|�@ٜ�@�F�@ؠ�@�@�9�@���@��N@� \@Ԟ@��W@�J�@�}�@�֡@с@σ{@ͣn@��@̳h@�'R@�[W@ʢ4@�Ft@��D@�f�@�bN@��@��@�;d@�Ĝ@ħ�@ĭ�@�m�@öF@�}�@�O�@�*0@�֡@I@�PH@�5?@�&�@��.@��)@���@�qv@�8@��@��@��@���@�<�@�ݘ@��-@�[W@��4@�7@���@�;d@���@���@��K@��s@�[�@�1'@�T�@���@�k�@���@���@���@���@�($@��@�;�@���@�\�@��6@���@���@���@��1@�p;@�2�@�p�@�&@���@���@��)@��@�Ta@�!�@���@���@��	@���@��@�\)@��@�{�@�u@���@�*0@��8@��x@�?@���@���@�N<@��	@��F@��;@��T@�خ@���@��n@�zx@�@��@���@�)�@��@�ϫ@���@��V@��t@���@��'@�n/@��@�y>@�b@���@��~@�A�@���@�@���@�9�@��"@��z@�u%@�R�@�2�@��@��@���@�y�@�6z@�@��@�]d@�b@���@�Mj@�֡@�C�@��o@��'@���@�O@�!�@�%@��[@�� @���@��7@��4@�O@�#�@��@���@���@�0U@��m@���@�>�@�u�@�@��@�w2@��@��z@�~�@�y>@�=q@�o�@��@���@��c@���@���@�ں@�4n@���@���@�iD@��@��@�e�@�PH@�<�@��@���@��a@��C@��V@��$@�~�@�E9@��@���@���@���@�q@�V�@�M@�B[@��D@��H@���@�:�@��"@��p@��9@���@�A�@�(�@��&@��@��@�g�@� \@� i@��@�ߤ@���@���@��4@��u@���@�a|@�0U@�@���@��@�qv@�%@��@���@���@��@���@�W�@���@���@��~@�Z�@�B�@�=@��@��]@��O@�@�� @���@�b�@��@��z@���@��1@���@�/�@���@�� @��n@�/@�	l@�%@��s@���@�r�@�YK@��@���@��@�e�@��@���@��@��2@�xl@�(�@��@���@�c@�b�@�Z�@�@��@�_�@�K^@��@��@W?@,�@~�,@~��@}c�@|�_@|Z@|�@{��@{~�@{�@{�@z�@z@�@y�@y�@xj@x	�@w��@v��@u�@u��@u<6@tg8@t~@s�0@s8@sS@r�F@r�@q��@qhs@q�@p��@p�@o�;@o��@n�@n��@nxl@n5?@m��@mzx@m�@l�@lZ@lI�@l�@k�*@k�@j� @i�@i�X@ic@iO�@h�5@h��@hq@hI�@f�]@f��@f:*@f:*@f#:@f@f_@e�z@e��@eo @e�@d֡@d��@dy>@c�	@b�@b�X@b4@ae,@ac�@a?}@a/@a�@`ѷ@`PH@_�{@^��@^@\�`@\tT@\�@[�Q@[�@[~�@[@O@Z�c@Z�@Zq�@Y��@X[�@W�;@W��@WU�@W��@W��@W��@WC@V=q@V�@U�@U�H@U�~@U5�@T��@T��@T/�@Ss@RW�@Q��@Q��@Qk�@P�@P|�@Pm�@P*�@Og�@N��@Nu%@Nff@Na|@N\�@NR�@NOv@NC�@N8�@Ne@MX@L4n@K�@K�@KX�@J�,@J��@J��@J�A@Ju%@Jn�@JL0@I�o@I�7@If�@IO�@IN<@I@H��@H[�@H1'@G��@F��@F�r@FTa@FGE@F-@F	@F{@F�@FJ@FJ@F	@F �@E�@E�X@D�@D/�@C�	@B��@B+k@A��@As�@A-w@@�@@oi@?��@>��@>@�@=�9@=hs@<�5@<U2@<4n@</�@<(�@<@<@<  @<�@;��@;��@;e�@:͟@:��@:i�@:kQ@:Z�@:#:@9��@9�@9�@8ѷ@8�Y@8~@7�K@7�@7��@7�@6ȴ@6z@6�@4�P@4�@4��@3��@3v`@3v`@3s@3Y@2͟@2�x@2H�@1�@1w2@1Y�@1G�@12a@1�@0��@/��@/��@/��@/��@/�F@/��@/�*@.��@.:*@-�d@-*0@,�4@,_@,<�@+�@+��@+"�@+o@*��@*��@*c @*($@*�@)�>@)��@)��@)e,@(�v@(Ĝ@(�@(�9@(��@(��@(Q�@(  @'� @'�a@'�@'�[@'��@'�V@'�k@'��@'��@'��@'o�@'A�@'!-@'@&�8@&ں@&�@%�M@%7L@% \@$�[@$�@$7@#�
@#�a@#��@#Mj@"�@"xl@"Ov@"-@"#:@"_@!�@!�@!��@!��@!�@!ԕ@!��@!�9@!ϫ@!��@ �@ ��@ tT@ g8@ _@ C-@ -�@�@�[@��@l�@4�@"�@�@�X@��@�\@xl@e@�X@[W@J�@Dg@/@�@�@�@��@C-@!@�@��@��@�@a|@�@#�@�@�@�E@�@~@�M@u%@@�@&�@	@_@��@��@��@�S@o @8�@�@�E@�$@��@�z@��@oi@e�@]d@2�@x@��@��@�k@n/@dZ@g�@a@W?@9�@&@�@҉@B[@ �@��@�@��@�=@Q�@(�@q@��@��@�.@�@j@M@7@�@�@��@خ@�6@��@�$@�	@C�@�@�@�@��@�2@�h@l�@\�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B
�?B
��B
�B
��B
�B
��B
��B
��B
��B
�+B
�B
�B
�B
�+B
�B
�B
�+B
�+B
�?B
��B
�B
��B
�mB
�SB
�SB
��B
��B
��B
��B
��B
��B
�gB
�[B
|�B
s�B
p�B
k6B
\�B
9�B
:�B
@ B
>�B
C{B
S&B
ZkB
r�B
wB
~�B
�uB
�B
��B
��B
�2B
�B
�DB
��B
�,B
��B �B�B
�!B
�B�B
�B
��B
��B
�B
��B
��B
�HB
ՁB
׍B
��B
�sB
�uB
ϑB
�+B
��B
� B
��B
��B
p�B
d�B
]�B
cB
AB
/B
zB	�B	��B	��B	��B	��B	sMB	nB	gmB	[qB	E�B	,WB	'�B	&2B	#�B	 \B	�B	
�B	B��B��B��B��B��B�B��B�B��B�*B�?B�B	�B��B��B��B�B�AB��B�jB�ZB��B�B�B	?B	�B	�B	B	B	�B	�B�B	�B	0B	7fB	:*B	B[B	FtB	F�B	a�B	bhB	9�B	$B�aB�8B��B��B��B��B�B�B�^B	-B	6B	�B	�B	�B	�B	5B	qB	�B	�B	
XB	�B	gB	�B	
�B	OB	�B	KB	B	�B	�B	 �B	�B	'�B	�B	�B	�B	+kB	+QB	3hB	D�B	i*B	k�B	jB	j�B	l"B	n/B	oB	o�B	p�B	y�B	~�B	w�B	p�B	p�B	w2B	w�B	xB	w�B	u�B	u�B	wfB	��B	�B	�#B	�dB	�6B	�&B	�<B	��B	��B	�B	��B	�hB	��B	�FB	�B	�qB	�fB	�WB	�wB	�0B	�B	�IB	��B	��B	�RB	��B	�B	��B	�.B	�<B	��B	�>B	�PB	��B	��B	�B	ȀB	��B	�KB	�B	�}B	�TB	��B	�,B	��B	�FB	�1B	��B	�kB	��B	��B	�EB	ՁB	уB	�.B	ϫB	�bB	��B	�vB	�BB	��B	�&B	��B	ߊB	�HB	��B	��B	�TB	�sB	��B	�B	�LB	�B	�hB	��B	�B	��B	ܒB	�pB	�;B	�B	�|B	��B	��B	�HB	οB	ΊB	�B	��B	��B	��B	�B	ևB	�B	��B	ּB	��B	�B	��B	�B	�7B	�B	�/B	�B	�pB	�;B	��B	�pB	�VB	ߊB	��B	�NB	��B	��B	�LB	�2B	�mB	�B	�B	�
B	�>B	�$B	�XB	��B	�
B	�B	��B	�2B	�FB	�zB	�B	�fB	�XB	��B	��B	�B	�OB	��B	�B	�UB	�]B	�KB	�"B	��B	��B	�B	��B	��B	�2B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	�6B	�B	��B	�0B	�dB	�dB	��B	�PB	�"B	�VB	��B	�]B	��B	�B	�cB	��B
 4B
 �B
�B
�B
3B
�B
B
9B
B
�B
�B
3B
�B
�B
SB
YB
tB
%B
�B
�B
�B
_B
�B
�B
�B
?B
B
MB
�B
�B
B
�B
MB
�B
B
�B
�B
�B
KB
	�B

�B

�B

#B

	B
	lB

	B

�B
�B
�B
dB
�B
\B
�B
HB
HB
�B
B
�B
B
&B
uB
@B
�B
MB
�B
�B
�B
YB
+B
EB
�B
B
�B
�B
B
�B
�B
qB
�B
qB
qB
qB
=B
�B
�B
B
�B
dB
�B
�B
�B
�B
�B
�B
�B
B
B
B
�B
B
�B
�B
�B
�B
B
OB
5B
�B
VB
 B
!-B
!�B
"hB
!�B
!�B
!�B
"4B
"NB
#B
"�B
#:B
#�B
$@B
$@B
$ZB
$@B
$ZB
$tB
$�B
$�B
$�B
$�B
$�B
%B
$�B
%,B
%�B
%�B
%�B
%`B
%�B
%�B
%�B
&B
&�B
'B
&�B
'mB
'�B
'mB
'�B
($B
'�B
'�B
'mB
'�B
'�B
($B
(�B
)DB
*�B
*�B
,WB
+kB
+�B
+�B
,�B
,�B
,�B
-wB
-�B
-wB
-B
-�B
-]B
-�B
-�B
.IB
./B
.B
-�B
.�B
.B
.�B
.�B
/B
0!B
1B
2-B
1�B
1�B
1�B
2GB
2|B
2�B
3�B
3�B
49B
3�B
3hB
2�B
33B
33B
3�B
4B
4�B
5tB
4�B
5%B
5�B
5�B
5�B
5�B
5�B
6B
6B
6FB
72B
7�B
9	B
9�B
9�B
:DB
:DB
:^B
:�B
;B
;JB
;�B
;�B
<6B
<�B
=B
=B
="B
=VB
=VB
=�B
>B
=�B
=�B
=�B
=�B
=�B
>�B
?B
>�B
?B
?.B
?}B
?�B
?}B
?cB
?�B
?�B
?�B
?�B
@iB
@�B
AB
A�B
A�B
BAB
B�B
B�B
B�B
B�B
C{B
DB
C�B
D�B
D�B
DgB
DgB
DMB
D3B
D3B
DMB
EB
D3B
D3B
DMB
D�B
D�B
EmB
E�B
E�B
FB
E�B
F%B
E�B
F?B
GEB
G+B
GEB
HfB
J=B
J�B
J�B
KDB
K�B
LdB
LdB
LdB
L�B
L�B
M6B
M6B
MPB
N<B
O(B
OBB
O�B
O\B
PHB
PB
O�B
O�B
PbB
P�B
Q4B
QNB
QhB
QhB
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
TB
TFB
T,B
T�B
UB
U2B
U2B
U2B
UMB
UMB
UgB
VB
V�B
V�B
V�B
V�B
W
B
W�B
W�B
W�B
X+B
YKB
Y�B
Y�B
Y�B
ZB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
Y�B
ZB
Z�B
Z�B
[WB
[�B
[�B
[�B
\xB
\]B
\�B
\�B
]�B
^B
^�B
_VB
_VB
`B
`vB
`�B
`\B
`�B
`�B
`�B
`�B
`vB
`�B
`�B
a-B
a�B
a�B
bB
a�B
b4B
b4B
b�B
b�B
c:B
cTB
cnB
c�B
dB
d&B
d&B
d�B
d�B
d�B
eB
f�B
f2B
fLB
g�B
g�B
g�B
g�B
h
B
h>B
hXB
h�B
iB
iDB
i_B
iDB
iDB
iDB
iDB
i�B
i�B
i�B
i�B
i�B
i�B
i�B
jB
j�B
jeB
jB
j0B
jB
j0B
jeB
jeB
jKB
j0B
j0B
jKB
jKB
jeB
jeB
jeB
j�B
j�B
k6B
k�B
k�B
lB
l"B
l"B
lWB
l�B
mCB
m�B
m�B
m�B
m�B
m�B
nB
nB
nB
n/B
nIB
ncB
n�B
n�B
n�B
n�B
n�B
o�B
p!B
pUB
poB
p�B
p�B
q�B
qvB
q�B
q�B
rB
r|B
r�B
r�B
r�B
r�B
sB
s3B
sB
s3B
s3B
s3B
s3B
sMB
s3B
r�B
sMB
s�B
t9B
t9B
tB
t9B
tTB
tTB
t�B
t�B
t�B
t�B
u?B
uB
utB
utB
u�B
u�B
u�B
vFB
v�B
wLB
wLB
wLB
wfB
wfB
w�B
xB
xB
x�B
x�B
x�B
y$B
yXB
y�B
zxB
{dB
{�B
{�B
{�B
{�B
{dB
{�B
}<B
}�B
}�B
}�B
}�B
}�B
~B
~(B
~BB
~]B
~wB
~�B
~�B
B
.B
.B
HB
}B
�B
}B
}B
�B
�B
�B
�B
�4B
�OB
�OB
�OB
�OB
�OB
�iB
�iB
��B
��B
�oB
��B
��B
��B
��B
��B
�[B
�[B
�'B
��B
��B
��B
�B
�B
�GB
�{B
�aB
��B
��B
��B
��B
��B
��B
��B
�MB
�MB
��B
��B
�MB
��B
��B
��B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B
�?B
��B
�B
��B
�B
��B
��B
��B
��B
�+B
�B
�B
�B
�+B
�B
�B
�+B
�+B
�?B
��B
�B
��B
�mB
�SB
�SB
��B
��B
��B
��B
��B
��B
�gB
�[B
|�B
s�B
p�B
k6B
\�B
9�B
:�B
@ B
>�B
C{B
S&B
ZkB
r�B
wB
~�B
�uB
�B
��B
��B
�2B
�B
�DB
��B
�,B
��B �B�B
�!B
�B�B
�B
��B
��B
�B
��B
��B
�HB
ՁB
׍B
��B
�sB
�uB
ϑB
�+B
��B
� B
��B
��B
p�B
d�B
]�B
cB
AB
/B
zB	�B	��B	��B	��B	��B	sMB	nB	gmB	[qB	E�B	,WB	'�B	&2B	#�B	 \B	�B	
�B	B��B��B��B��B��B�B��B�B��B�*B�?B�B	�B��B��B��B�B�AB��B�jB�ZB��B�B�B	?B	�B	�B	B	B	�B	�B�B	�B	0B	7fB	:*B	B[B	FtB	F�B	a�B	bhB	9�B	$B�aB�8B��B��B��B��B�B�B�^B	-B	6B	�B	�B	�B	�B	5B	qB	�B	�B	
XB	�B	gB	�B	
�B	OB	�B	KB	B	�B	�B	 �B	�B	'�B	�B	�B	�B	+kB	+QB	3hB	D�B	i*B	k�B	jB	j�B	l"B	n/B	oB	o�B	p�B	y�B	~�B	w�B	p�B	p�B	w2B	w�B	xB	w�B	u�B	u�B	wfB	��B	�B	�#B	�dB	�6B	�&B	�<B	��B	��B	�B	��B	�hB	��B	�FB	�B	�qB	�fB	�WB	�wB	�0B	�B	�IB	��B	��B	�RB	��B	�B	��B	�.B	�<B	��B	�>B	�PB	��B	��B	�B	ȀB	��B	�KB	�B	�}B	�TB	��B	�,B	��B	�FB	�1B	��B	�kB	��B	��B	�EB	ՁB	уB	�.B	ϫB	�bB	��B	�vB	�BB	��B	�&B	��B	ߊB	�HB	��B	��B	�TB	�sB	��B	�B	�LB	�B	�hB	��B	�B	��B	ܒB	�pB	�;B	�B	�|B	��B	��B	�HB	οB	ΊB	�B	��B	��B	��B	�B	ևB	�B	��B	ּB	��B	�B	��B	�B	�7B	�B	�/B	�B	�pB	�;B	��B	�pB	�VB	ߊB	��B	�NB	��B	��B	�LB	�2B	�mB	�B	�B	�
B	�>B	�$B	�XB	��B	�
B	�B	��B	�2B	�FB	�zB	�B	�fB	�XB	��B	��B	�B	�OB	��B	�B	�UB	�]B	�KB	�"B	��B	��B	�B	��B	��B	�2B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	�6B	�B	��B	�0B	�dB	�dB	��B	�PB	�"B	�VB	��B	�]B	��B	�B	�cB	��B
 4B
 �B
�B
�B
3B
�B
B
9B
B
�B
�B
3B
�B
�B
SB
YB
tB
%B
�B
�B
�B
_B
�B
�B
�B
?B
B
MB
�B
�B
B
�B
MB
�B
B
�B
�B
�B
KB
	�B

�B

�B

#B

	B
	lB

	B

�B
�B
�B
dB
�B
\B
�B
HB
HB
�B
B
�B
B
&B
uB
@B
�B
MB
�B
�B
�B
YB
+B
EB
�B
B
�B
�B
B
�B
�B
qB
�B
qB
qB
qB
=B
�B
�B
B
�B
dB
�B
�B
�B
�B
�B
�B
�B
B
B
B
�B
B
�B
�B
�B
�B
B
OB
5B
�B
VB
 B
!-B
!�B
"hB
!�B
!�B
!�B
"4B
"NB
#B
"�B
#:B
#�B
$@B
$@B
$ZB
$@B
$ZB
$tB
$�B
$�B
$�B
$�B
$�B
%B
$�B
%,B
%�B
%�B
%�B
%`B
%�B
%�B
%�B
&B
&�B
'B
&�B
'mB
'�B
'mB
'�B
($B
'�B
'�B
'mB
'�B
'�B
($B
(�B
)DB
*�B
*�B
,WB
+kB
+�B
+�B
,�B
,�B
,�B
-wB
-�B
-wB
-B
-�B
-]B
-�B
-�B
.IB
./B
.B
-�B
.�B
.B
.�B
.�B
/B
0!B
1B
2-B
1�B
1�B
1�B
2GB
2|B
2�B
3�B
3�B
49B
3�B
3hB
2�B
33B
33B
3�B
4B
4�B
5tB
4�B
5%B
5�B
5�B
5�B
5�B
5�B
6B
6B
6FB
72B
7�B
9	B
9�B
9�B
:DB
:DB
:^B
:�B
;B
;JB
;�B
;�B
<6B
<�B
=B
=B
="B
=VB
=VB
=�B
>B
=�B
=�B
=�B
=�B
=�B
>�B
?B
>�B
?B
?.B
?}B
?�B
?}B
?cB
?�B
?�B
?�B
?�B
@iB
@�B
AB
A�B
A�B
BAB
B�B
B�B
B�B
B�B
C{B
DB
C�B
D�B
D�B
DgB
DgB
DMB
D3B
D3B
DMB
EB
D3B
D3B
DMB
D�B
D�B
EmB
E�B
E�B
FB
E�B
F%B
E�B
F?B
GEB
G+B
GEB
HfB
J=B
J�B
J�B
KDB
K�B
LdB
LdB
LdB
L�B
L�B
M6B
M6B
MPB
N<B
O(B
OBB
O�B
O\B
PHB
PB
O�B
O�B
PbB
P�B
Q4B
QNB
QhB
QhB
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
TB
TFB
T,B
T�B
UB
U2B
U2B
U2B
UMB
UMB
UgB
VB
V�B
V�B
V�B
V�B
W
B
W�B
W�B
W�B
X+B
YKB
Y�B
Y�B
Y�B
ZB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
Y�B
ZB
Z�B
Z�B
[WB
[�B
[�B
[�B
\xB
\]B
\�B
\�B
]�B
^B
^�B
_VB
_VB
`B
`vB
`�B
`\B
`�B
`�B
`�B
`�B
`vB
`�B
`�B
a-B
a�B
a�B
bB
a�B
b4B
b4B
b�B
b�B
c:B
cTB
cnB
c�B
dB
d&B
d&B
d�B
d�B
d�B
eB
f�B
f2B
fLB
g�B
g�B
g�B
g�B
h
B
h>B
hXB
h�B
iB
iDB
i_B
iDB
iDB
iDB
iDB
i�B
i�B
i�B
i�B
i�B
i�B
i�B
jB
j�B
jeB
jB
j0B
jB
j0B
jeB
jeB
jKB
j0B
j0B
jKB
jKB
jeB
jeB
jeB
j�B
j�B
k6B
k�B
k�B
lB
l"B
l"B
lWB
l�B
mCB
m�B
m�B
m�B
m�B
m�B
nB
nB
nB
n/B
nIB
ncB
n�B
n�B
n�B
n�B
n�B
o�B
p!B
pUB
poB
p�B
p�B
q�B
qvB
q�B
q�B
rB
r|B
r�B
r�B
r�B
r�B
sB
s3B
sB
s3B
s3B
s3B
s3B
sMB
s3B
r�B
sMB
s�B
t9B
t9B
tB
t9B
tTB
tTB
t�B
t�B
t�B
t�B
u?B
uB
utB
utB
u�B
u�B
u�B
vFB
v�B
wLB
wLB
wLB
wfB
wfB
w�B
xB
xB
x�B
x�B
x�B
y$B
yXB
y�B
zxB
{dB
{�B
{�B
{�B
{�B
{dB
{�B
}<B
}�B
}�B
}�B
}�B
}�B
~B
~(B
~BB
~]B
~wB
~�B
~�B
B
.B
.B
HB
}B
�B
}B
}B
�B
�B
�B
�B
�4B
�OB
�OB
�OB
�OB
�OB
�iB
�iB
��B
��B
�oB
��B
��B
��B
��B
��B
�[B
�[B
�'B
��B
��B
��B
�B
�B
�GB
�{B
�aB
��B
��B
��B
��B
��B
��B
��B
�MB
�MB
��B
��B
�MB
��B
��B
��B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20230718184319  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8b                                                                20230718184356  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20230718184357  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20230718184357                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20230718184358  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20230718184358  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20230718185725                      G�O�G�O�G�O�                