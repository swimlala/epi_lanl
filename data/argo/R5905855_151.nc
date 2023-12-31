CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2023-04-04T00:49:49Z creation;2023-04-04T00:49:50Z conversion to V3.1      
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
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20230404004949  20230404010516  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @� ��w؏1   @� �U��	@/�/��w�c�A�7K�1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @9��@y��@�  A   AffA@  A`  A�  A�  A�  A���A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8ffB@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�ffB�33B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B뙚B�  B�  B�  B�  C   C  C  C  C  C	�fC  C  C�C�C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@�CB  CD  CF  CH  CJ  CL�CM�fCP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D%��D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1�fD2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv�fDw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @5�@u�@�@�AG�A>�HA^�HA~�HA�p�A�p�A�=qA�p�A�p�A�p�A�p�A�p�B�RB�RB�RB�RB'�RB/�RB8�B?�RBG�RBO�RBW�RB_�RBg�RBo�RBw�RB�RB��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B�\B�B�B�\B��)B���B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��B�u�B��)B��)B��)B��)B��)C�C�C�C�C	�zC�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C@�CA�CC�CE�CG�CI�CL�CM�zCO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��=C��
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
C��
C��
C��
C��
C��
C��
C��
D {�D ��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D	{�D	��D
{�D
��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D {�D ��D!{�D!��D"{�D"��D#{�D#��D${�D$��D%{�D%�D&{�D&��D'{�D'��D({�D(��D){�D)��D*{�D*��D+{�D+��D,{�D,��D-{�D-��D.{�D.��D/{�D/��D0{�D0��D1��D1��D2{�D2��D3{�D3��D4{�D4��D5{�D5��D6{�D6��D7{�D7��D8{�D8��D9{�D9��D:{�D:��D;{�D;��D<{�D<��D={�D=��D>{�D>��D?{�D?��D@{�D@��DA{�DA��DB{�DB��DC{�DC��DD{�DD��DE{�DE��DF{�DF��DG{�DG��DH{�DH��DI{�DI��DJ{�DJ��DK{�DK��DL{�DL��DM{�DM��DN{�DN��DO{�DO��DP{�DP��DQ{�DQ��DR{�DR��DS{�DS��DT{�DT��DU{�DU��DV{�DV��DW{�DW��DX{�DX��DY{�DY��DZ{�DZ��D[{�D[��D\{�D\��D]{�D]��D^{�D^��D_{�D_��D`{�D`��Da{�Da��Db{�Db��Dc{�Dc��Dd{�Dd��De{�De��Df{�Df��Dg{�Dg��Dh{�Dh��Di{�Di��Dj{�Dj��Dk{�Dk��Dl{�Dl��Dm{�Dm��Dn{�Dn��Do{�Do��Dp{�Dp��Dq{�Dq��Dr{�Dr��Ds{�Ds��Dt{�Dt��Du{�Du��Dv��Dv��Dw{�Dw��Dx{�Dx��Dy{�Dy��Dz{�Dz��D{{�D{��D|{�D|��D}{�D}��D~{�D~��D{�D��D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D���D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D½�D���D�=�D�}�Dý�D���D�=�D�}�DĽ�D���D�=�D�}�DŽ�D���D�=�D�}�Dƽ�D���D�=�D�}�Dǽ�D���D�=�D�}�DȽ�D���D�=�D�}�Dɽ�D���D�=�D�}�Dʽ�D���D�=�D�}�D˽�D���D�=�D�}�D̽�D���D�=�D�}�Dͽ�D���D�=�D�}�Dν�D���D�=�D�}�DϽ�D���D�=�D�}�Dн�D���D�=�D�}�Dѽ�D���D�=�D�}�Dҽ�D���D�=�D�}�Dӽ�D���D�=�D�}�DԽ�D���D�=�D�}�Dս�D���D�=�D�}�Dֽ�D���D�=�D�}�D׽�D���D�=�D�}�Dؽ�D���D�=�D�}�Dٽ�D���D�=�D�}�Dڽ�D���D�=�D�}�D۽�D���D�=�D�}�Dܽ�D���D�=�D�}�Dݽ�D���D�=�D�}�D޽�D���D�=�D�}�D߽�D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D���D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�:�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�یA��TA���A���A��NA��&A��Aϼ�AϼjAϽ�AϹ$AϹ$Aϸ�AϹ�AϺ^AϺ^AϺ�AϹ$Aϰ�Aϯ�AϮ}Aϟ�Aλ0A̛qA�aA��AA�҉Aʞ�A�iDA�V�A�"4AɮA�($A�G�AǙ�A�qAA�f2A�ffA�d�A�c�A�)�A��A�FA�'�A�8�A�5�A�,A�,�A�@�A�.IA���A�5?Aò-A�D�A���A�9XA���A�m�A��0A��9A���A���A���A��2A�>A�XEA��OA�I�A�$tA��IA��>A�m�A�+kA��=A�HA���A��A��A��=A�a|A�n�A�A��A�	A�5�A��8A��bA��/A�L�A�E�A���A���A�N<A���A�2-A��$A���A���A�.A��A�6FA~��A|ApY�Ai�Ag��Af}�Ae�_Ad�AcɆA^��A\�AWqAT�AP�DAO�-AN4AK�HAIG�AF��AEGAB� A@=�A>�A<ߤA9�uA7%A5�XA4�A4n/A4B[A3�A0�AA/�A-�A+VA*>�A)W?A(��A&YA$;�A"�AL0Ax�AcA�KAGEAߤA�A��AOA�rA��A�A�-A�5A
�#A
5?A��A�UA��A�A�'A�FAGEAU�AںA��A�oAg�AXyA�AV�A ߤA @O@��@�F@�S&@��@�+�@��Y@��@�qv@�%F@��b@�7@�;�@�@@���@�@�;d@��@�C@�<6@��@��@�l�@�K�@�e�@��@��W@�@��t@�a|@�]�@��9@���@�d�@��@�@�6@�Q�@��@�@⎊@���@�:�@ߋ�@�H@�|�@ܒ�@��@�ƨ@�x�@�!@���@�33@�x@�:�@֏\@�5?@��d@Ղ�@�o @���@�֡@�u%@��@Ӳ-@�c�@Ծ@�Z@��@�C�@үO@�rG@�X�@�C@��@Ң4@�kQ@ϊ	@��Q@ͩ�@Ͱ�@ζ�@�c@�'�@��@��@��@�PH@�@���@ͽ�@͐�@�/@���@�=q@���@�S�@ʊr@��d@�O�@�&�@���@ǎ�@��@ƦL@�_@�:�@�!�@ż�@���@�M�@�}�@�@���@�@�Z@��@��C@�a@��v@�tT@�!�@���@�ϫ@���@�rG@�@O@�Y@��@�3�@��*@�;@��@�Z�@��@��x@���@�RT@�#�@�@@��@��@�@��M@���@�?�@� �@�ݘ@���@��@�#:@���@��@��@���@�O@�'R@�E9@�+@��c@��$@��@�V@�9X@�6@�-�@��o@��@���@�`B@���@�[�@��@���@���@�j�@�33@��@��|@��@���@�)�@���@��@��@���@�_�@�;�@�%�@�0�@�͟@���@�w�@�1@�}�@�e�@���@�"h@���@���@�S�@��L@�/�@� �@��w@�P�@��@���@��Y@�Ov@��@���@���@�33@�ȴ@���@��@�K�@�{�@��@���@��@���@�!@���@���@�[W@��H@�H�@���@��*@�}�@�6z@���@���@�\�@�1'@���@�@�~�@��@���@��<@��+@�@� �@��)@��t@�e�@��@��L@�j@�.�@��+@��P@�J#@��@��@���@���@�L0@���@��~@�8�@���@��4@�q�@�.�@���@��z@���@�s@���@�O@� �@�	�@�K�@��P@���@�S�@��.@�b�@��@�e@�l�@��.@�	�@��u@�֡@�*�@��-@�x@���@���@�{�@�V@�Xy@�\�@���@��9@���@���@�@�@��@� �@��@�� @��'@���@�l�@�RT@�C@�͟@���@��L@�p;@�M�@�&�@���@���@�G�@�5�@�%F@��5@���@���@�{�@�1�@��@��@��;@���@���@�>�@��@�ں@��.@��D@��3@���@���@�x@�iD@�a@�P�@�$t@���@��@��o@�[�@���@�Q�@�n�@�.�@��@˒@t�@W?@~�@}��@} \@|�o@|V�@{��@z��@z�h@zl�@z&�@z�@y�3@y|@x�K@x��@x[�@x-�@w�@w�&@wݘ@wl�@v��@vz@v�@u��@u�7@u4@t��@tw�@s�]@s� @s��@s(@ra|@r{@q�M@qL�@q@p��@p_@p�@o��@oiD@oJ#@o�@n�H@nff@nL0@m�.@m�t@m��@mO�@m;@l��@l*�@lb@k��@kg�@kY@j�!@j�@i��@i�@i;@h>B@g�F@g@f҉@f�@e�@e=�@d�5@d��@d��@du�@d1'@d  @c��@cY@b�@bں@bn�@bO@a�C@aB�@`�@`�@_�0@_��@_��@_s@_A�@_+@_�@^ߤ@^��@^H�@^�@]�Z@]��@]�7@][W@]A @\��@\g8@\�@[g�@Z��@Y��@Yc@Yk�@Ya�@Y�@X��@X��@XtT@X%�@W��@Wg�@V�R@VM�@U��@U@U�@T�?@Th�@S�&@S�w@S�k@S{J@Sj�@S\)@S;d@Ra|@Q|@P��@P��@PU2@O�]@OE9@N��@NYK@NR�@NE�@N3�@N{@M�D@Mԕ@M!�@L��@LU2@K�V@K6z@J�y@Js�@JH�@J_@I�>@I�z@Ik�@I�@H��@H~@G˒@G��@G�@G~�@Gj�@F�"@F�L@FOv@E��@E#�@D�@D�_@D]d@D~@C��@C�@@CU�@C�@B�h@B@�@B_@A�^@A��@A=�@@��@@�9@@�I@@]d@?�6@?�P@?K�@>�@>B[@> �@=��@=��@=��@=7L@<�@<l"@<N�@<'R@<1@;�@;��@;\)@;�@:=q@9��@9��@9w2@95�@8�$@8j@7��@7�w@7~�@7�@6ߤ@6��@6l�@6R�@5��@5+@4��@4q@4:�@3�@3�F@3��@3iD@3o@2�,@2��@2��@1�N@1��@1�"@1�@1e,@1A @1�@0�v@0��@0oi@01'@/�0@/E9@.�B@.��@.� @-��@-zx@-\�@-L�@-/@,�	@,�9@,��@,r�@,S�@,2�@,x@+˒@+/�@+�@*�,@*��@*��@*h
@*YK@*!�@*u@)�@)��@)zx@)N<@)%F@(�5@(��@(u�@(Ft@(	�@'��@'A�@&��@&��@&h
@&B[@&{@%�Z@%�9@%��@%IR@$�f@$�p@$��@$Z@$2�@$�@#�+@#�@#��@#b�@#=@#Y@"�@"�@"�@!ϫ@!��@!!�@ �5@ Ɇ@ �@ ��@ h�@ PH@ $@ b@��@˒@��@S�@C@�"@��@��@u%@@�Z@��@��@�@�@Ĝ@�u@tT@:�@�m@��@v`@W?@/�@��@z@Ov@-@	@��@�@��@T�@?}@�@�f@�`@��@Q�@ݘ@iD@�@��@��@p;@h
@W�@{@��@�@J�@�f@�@�)@��@�@bN@H@@�@�4@Z�@/�@
=@ȴ@}V@V@&�@��@��@�@�-@�X@�M@Vm@Dg@-w@�@ی@��@��@_@S�@I�@$@��@s@g�@J#@�@�!@��@0U@��@�-@�h@hs@Y�@S&@O�@B�@?}@5�@��@��@w�@K^@��@��@��@�$@��@��@o�@>�@�@�@
��@
��@
��@
�@
z@
{�@
^5@
6�@
#:@
�@
 �@	�d@	�n@	a�@��@��@�_@H@4n@,=@$@�@�@�r@�}@�0@��@�[@]�@�"@��@��@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�یA��TA���A���A��NA��&A��Aϼ�AϼjAϽ�AϹ$AϹ$Aϸ�AϹ�AϺ^AϺ^AϺ�AϹ$Aϰ�Aϯ�AϮ}Aϟ�Aλ0A̛qA�aA��AA�҉Aʞ�A�iDA�V�A�"4AɮA�($A�G�AǙ�A�qAA�f2A�ffA�d�A�c�A�)�A��A�FA�'�A�8�A�5�A�,A�,�A�@�A�.IA���A�5?Aò-A�D�A���A�9XA���A�m�A��0A��9A���A���A���A��2A�>A�XEA��OA�I�A�$tA��IA��>A�m�A�+kA��=A�HA���A��A��A��=A�a|A�n�A�A��A�	A�5�A��8A��bA��/A�L�A�E�A���A���A�N<A���A�2-A��$A���A���A�.A��A�6FA~��A|ApY�Ai�Ag��Af}�Ae�_Ad�AcɆA^��A\�AWqAT�AP�DAO�-AN4AK�HAIG�AF��AEGAB� A@=�A>�A<ߤA9�uA7%A5�XA4�A4n/A4B[A3�A0�AA/�A-�A+VA*>�A)W?A(��A&YA$;�A"�AL0Ax�AcA�KAGEAߤA�A��AOA�rA��A�A�-A�5A
�#A
5?A��A�UA��A�A�'A�FAGEAU�AںA��A�oAg�AXyA�AV�A ߤA @O@��@�F@�S&@��@�+�@��Y@��@�qv@�%F@��b@�7@�;�@�@@���@�@�;d@��@�C@�<6@��@��@�l�@�K�@�e�@��@��W@�@��t@�a|@�]�@��9@���@�d�@��@�@�6@�Q�@��@�@⎊@���@�:�@ߋ�@�H@�|�@ܒ�@��@�ƨ@�x�@�!@���@�33@�x@�:�@֏\@�5?@��d@Ղ�@�o @���@�֡@�u%@��@Ӳ-@�c�@Ծ@�Z@��@�C�@үO@�rG@�X�@�C@��@Ң4@�kQ@ϊ	@��Q@ͩ�@Ͱ�@ζ�@�c@�'�@��@��@��@�PH@�@���@ͽ�@͐�@�/@���@�=q@���@�S�@ʊr@��d@�O�@�&�@���@ǎ�@��@ƦL@�_@�:�@�!�@ż�@���@�M�@�}�@�@���@�@�Z@��@��C@�a@��v@�tT@�!�@���@�ϫ@���@�rG@�@O@�Y@��@�3�@��*@�;@��@�Z�@��@��x@���@�RT@�#�@�@@��@��@�@��M@���@�?�@� �@�ݘ@���@��@�#:@���@��@��@���@�O@�'R@�E9@�+@��c@��$@��@�V@�9X@�6@�-�@��o@��@���@�`B@���@�[�@��@���@���@�j�@�33@��@��|@��@���@�)�@���@��@��@���@�_�@�;�@�%�@�0�@�͟@���@�w�@�1@�}�@�e�@���@�"h@���@���@�S�@��L@�/�@� �@��w@�P�@��@���@��Y@�Ov@��@���@���@�33@�ȴ@���@��@�K�@�{�@��@���@��@���@�!@���@���@�[W@��H@�H�@���@��*@�}�@�6z@���@���@�\�@�1'@���@�@�~�@��@���@��<@��+@�@� �@��)@��t@�e�@��@��L@�j@�.�@��+@��P@�J#@��@��@���@���@�L0@���@��~@�8�@���@��4@�q�@�.�@���@��z@���@�s@���@�O@� �@�	�@�K�@��P@���@�S�@��.@�b�@��@�e@�l�@��.@�	�@��u@�֡@�*�@��-@�x@���@���@�{�@�V@�Xy@�\�@���@��9@���@���@�@�@��@� �@��@�� @��'@���@�l�@�RT@�C@�͟@���@��L@�p;@�M�@�&�@���@���@�G�@�5�@�%F@��5@���@���@�{�@�1�@��@��@��;@���@���@�>�@��@�ں@��.@��D@��3@���@���@�x@�iD@�a@�P�@�$t@���@��@��o@�[�@���@�Q�@�n�@�.�@��@˒@t�@W?@~�@}��@} \@|�o@|V�@{��@z��@z�h@zl�@z&�@z�@y�3@y|@x�K@x��@x[�@x-�@w�@w�&@wݘ@wl�@v��@vz@v�@u��@u�7@u4@t��@tw�@s�]@s� @s��@s(@ra|@r{@q�M@qL�@q@p��@p_@p�@o��@oiD@oJ#@o�@n�H@nff@nL0@m�.@m�t@m��@mO�@m;@l��@l*�@lb@k��@kg�@kY@j�!@j�@i��@i�@i;@h>B@g�F@g@f҉@f�@e�@e=�@d�5@d��@d��@du�@d1'@d  @c��@cY@b�@bں@bn�@bO@a�C@aB�@`�@`�@_�0@_��@_��@_s@_A�@_+@_�@^ߤ@^��@^H�@^�@]�Z@]��@]�7@][W@]A @\��@\g8@\�@[g�@Z��@Y��@Yc@Yk�@Ya�@Y�@X��@X��@XtT@X%�@W��@Wg�@V�R@VM�@U��@U@U�@T�?@Th�@S�&@S�w@S�k@S{J@Sj�@S\)@S;d@Ra|@Q|@P��@P��@PU2@O�]@OE9@N��@NYK@NR�@NE�@N3�@N{@M�D@Mԕ@M!�@L��@LU2@K�V@K6z@J�y@Js�@JH�@J_@I�>@I�z@Ik�@I�@H��@H~@G˒@G��@G�@G~�@Gj�@F�"@F�L@FOv@E��@E#�@D�@D�_@D]d@D~@C��@C�@@CU�@C�@B�h@B@�@B_@A�^@A��@A=�@@��@@�9@@�I@@]d@?�6@?�P@?K�@>�@>B[@> �@=��@=��@=��@=7L@<�@<l"@<N�@<'R@<1@;�@;��@;\)@;�@:=q@9��@9��@9w2@95�@8�$@8j@7��@7�w@7~�@7�@6ߤ@6��@6l�@6R�@5��@5+@4��@4q@4:�@3�@3�F@3��@3iD@3o@2�,@2��@2��@1�N@1��@1�"@1�@1e,@1A @1�@0�v@0��@0oi@01'@/�0@/E9@.�B@.��@.� @-��@-zx@-\�@-L�@-/@,�	@,�9@,��@,r�@,S�@,2�@,x@+˒@+/�@+�@*�,@*��@*��@*h
@*YK@*!�@*u@)�@)��@)zx@)N<@)%F@(�5@(��@(u�@(Ft@(	�@'��@'A�@&��@&��@&h
@&B[@&{@%�Z@%�9@%��@%IR@$�f@$�p@$��@$Z@$2�@$�@#�+@#�@#��@#b�@#=@#Y@"�@"�@"�@!ϫ@!��@!!�@ �5@ Ɇ@ �@ ��@ h�@ PH@ $@ b@��@˒@��@S�@C@�"@��@��@u%@@�Z@��@��@�@�@Ĝ@�u@tT@:�@�m@��@v`@W?@/�@��@z@Ov@-@	@��@�@��@T�@?}@�@�f@�`@��@Q�@ݘ@iD@�@��@��@p;@h
@W�@{@��@�@J�@�f@�@�)@��@�@bN@H@@�@�4@Z�@/�@
=@ȴ@}V@V@&�@��@��@�@�-@�X@�M@Vm@Dg@-w@�@ی@��@��@_@S�@I�@$@��@s@g�@J#@�@�!@��@0U@��@�-@�h@hs@Y�@S&@O�@B�@?}@5�@��@��@w�@K^@��@��@��@�$@��@��@o�@>�@�@�@
��@
��@
��@
�@
z@
{�@
^5@
6�@
#:@
�@
 �@	�d@	�n@	a�@��@��@�_@H@4n@,=@$@�@�@�r@�}@�0@��@�[@]�@�"@��@��@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B
*KB
)�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
)_B
)DB
)B
(�B
'B
!�B
�B
AB	�B	�}B
�B
l=B
�~B
��B
�B
��B
��B
��B
ÖB
ϫB
�NB
��B
��BjB<B�B$&B1vB5%BS[B��B��B��B�>B�B�B��BǮB�sB�=B�KB�B��B�sB�6B�B�B�B�B�=B�[B��B�tB�B�B�B�B�_BʦB� B�?B��B��B�MB��BvzB]~B<B�B�B%B
�DB
�B
�"B
�pB
pB
X_B
M�B
GzB
<�B
-B
*�B
 �B
B
B	�CB	��B	��B	~wB	w�B	q�B	m)B	e,B	P�B	B'B	@OB	5%B	'�B	$�B	B	hB	B�?B�B��B�B��B� BˬBʦB��B�XBȀB�+B��B�}B�B�*B�B�?B�B��B�FB��B�TB�lB��B��B|�By�BtBr-Bq�Bp�Bo�Bn}Bv`B~]B��B��B�UB��B�oB��B�B�B��B�dB��B��B��B�|B�-B��B��B�:B��B�nB��B��B�FB��B�B�"B�)B�/B��B��B�iB��B�vB�B��B�B�CB��BΥB�B	)B	
�B	�B	�B	�B	�B	5B	8lB	6zB	="B	>(B	@�B	F%B	K�B	R�B	\)B	]IB	\�B	]IB	_pB	`�B	a|B	dZB	f�B	h
B	i�B	k�B	l�B	o�B	t�B	v`B	w�B	z^B	}qB	�B	��B	��B	�YB	�EB	��B	��B	�MB	��B	��B	�mB	�UB	�IB	��B	��B	�6B	�[B	�hB	��B	�	B	�RB	�B	��B	�cB	�B	��B	��B	ƎB	�1B	��B	�B	�B	͹B	�"B	οB	�BB	�BB	��B	��B	ѝB	�4B	��B	ЗB	��B	��B	��B	�B	��B	�,B	�aB	��B	�2B	�B	՛B	�SB	׍B	�B	ٚB	ٚB	��B	��B	�7B	�#B	��B	��B	�dB	ݘB	��B	ݲB	ݲB	��B	��B	��B	�5B	ޞB	�OB	�VB	�\B	�BB	�pB	ߊB	�BB	�B	�-B	� B	�B	�fB	�RB	�B	�B	�>B	��B	�B	�B	�B	�FB	�LB	��B	�B	�_B	��B	�XB	�
B	�B	�$B	��B	��B	�B	��B	�B	�B	�IB	�B	�B	��B	�%B	��B	�LB	�2B	�RB	�8B	�lB	�	B	�XB	�XB	��B	�rB	��B	��B	��B	��B	�rB	��B	�6B	��B	�6B	��B	�wB	�HB
  B
  B
�B
�B
�B
aB
�B
�B
?B
�B
_B
1B
1B
�B
�B
�B
	�B
	�B
	�B

#B

�B
0B
�B
�B
JB
�B
^B
�B
B
B
VB
�B
B
�B
bB
bB
bB
}B
 B
�B
B
�B
�B
uB
&B
[B
�B
uB
FB
{B
�B
2B
�B
�B
B
$B
$B
�B
mB
�B
�B
mB
mB
�B
�B
�B
sB
�B
�B
KB
�B
	B
�B
�B
�B
)B
jB
�B
"NB
#TB
)DB
*�B
+�B
+kB
+B
*B
)*B
(>B
%�B
&fB
&�B
#�B
"�B
%�B
'�B
$�B
!�B
#�B
!bB
"�B
$B
#�B
$&B
$�B
)*B
,�B
.B
/�B
0oB
0�B
1[B
2|B
3�B
49B
4�B
4�B
4�B
5�B
6zB
6zB
6`B
6�B
7B
7LB
72B
72B
7�B
7�B
7�B
7�B
7�B
7�B
7�B
8B
88B
8lB
8lB
8�B
8�B
9>B
9rB
9�B
:*B
:*B
;dB
<PB
<jB
<jB
<�B
<�B
<�B
<�B
=B
<�B
<jB
;�B
:^B
8�B
7�B
7�B
8RB
8�B
8�B
8�B
9rB
;0B
:�B
:DB
:B
9�B
:�B
<B
<�B
=�B
=�B
=�B
>B
>BB
=�B
=qB
=<B
<�B
<jB
<�B
<�B
<�B
=B
=�B
=�B
=�B
=�B
>B
>BB
>wB
>�B
>�B
?B
?�B
?�B
@4B
@OB
@�B
@�B
@�B
A;B
A�B
A�B
A�B
A�B
A�B
B�B
B[B
B�B
B�B
CB
C-B
CaB
C�B
C�B
C�B
D3B
DgB
D�B
EB
E�B
E�B
E�B
E�B
F�B
F�B
GEB
GEB
GzB
G�B
H�B
IB
IRB
IRB
IlB
I�B
I�B
I�B
JrB
JXB
JrB
J�B
J�B
KB
KDB
K�B
L0B
LdB
LdB
LdB
L~B
L�B
L�B
L�B
L�B
MB
M6B
MPB
MjB
M�B
M�B
M�B
M�B
M�B
N"B
NVB
N�B
O\B
PB
P.B
PB
O�B
PbB
P}B
P�B
P�B
P�B
P�B
Q4B
Q�B
R:B
R�B
S&B
R�B
S�B
TFB
T�B
T�B
T�B
U2B
UMB
U�B
U�B
VmB
V�B
WYB
W�B
X+B
XyB
YB
Y�B
Y�B
Y�B
Y�B
Y�B
Y�B
Y�B
Y�B
Z�B
Z�B
Z�B
[�B
[�B
[�B
\CB
\CB
\xB
\xB
\�B
\�B
\�B
]IB
]�B
]�B
]�B
^B
^B
^B
^jB
^�B
^�B
_VB
_�B
_�B
_�B
`B
`BB
`\B
`vB
`�B
`�B
aB
abB
a|B
a�B
a�B
a�B
b4B
b4B
b4B
bNB
b�B
b�B
b�B
c B
c�B
c�B
c�B
c�B
c�B
dB
dZB
d�B
d�B
d�B
d�B
d�B
d�B
d�B
eB
e�B
fB
e�B
fLB
f�B
f�B
gB
g�B
g�B
g�B
hXB
hXB
h�B
h�B
h�B
iDB
jB
jKB
jKB
jeB
jB
j�B
jB
j�B
kB
k�B
k�B
k�B
lWB
lB
l"B
l"B
l"B
l"B
l=B
lqB
lqB
l�B
l�B
mB
m�B
nIB
n/B
ncB
o B
o B
oB
oB
o5B
oOB
o�B
o�B
p!B
pB
p!B
p!B
p;B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
rB
rB
raB
r|B
r|B
r�B
r�B
r�B
s3B
s3B
sMB
s�B
s�B
tB
t9B
t�B
t�B
t�B
t�B
t�B
t�B
u?B
utB
u�B
u�B
u�B
v+B
vFB
v`B
vzB
v�B
v�B
v�B
v�B
v�B
wB
wfB
w�B
w�B
xB
xlB
x�B
x�B
x�B
x�B
y$B
y	B
yXB
yXB
yrB
y�B
y�B
y�B
z*B
zDB
zxB
zxB
z�B
{B
z�B
{B
{B
{�B
{�B
{�B
{�B
{�B
|B
|jB
|jB
|�B
|�B
|�B
}B
}VB
}qB
}�B
}�B
}�B
}�B
}�B
~B
~(B
~]B
~]B
~wB
~wB
~�B
HB
�B
�B
�OB
��B
��B
��B
��B
��B
�B
� B
��B
��B
��B
��B
�'B
�AB
�[B
�[B
��B
��B
��B
�B
�B
�-B
�aB
��B
��B
��B
�B
�B
�MB
�MB
�MB
��B
��B
��B
��B
�B
�9B
��B
��B
��B
��B
��B
��B
�YB
��B
��B
��B
�+B
�_B
�_B
��B
�1B
�KB
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�	B
�XB
�XB
��B
��B
��B
��B
��B
��B
�)B
�DB
�xB
��B
��B
��B
��B
��B
�B
�JB
�JB
�dB
�dB
�~B
��B
��B
�PB
��B
��B
�"B
�B
�"B
�"B
�<B
�"B
�VB
�pB
��B
��B
�VB
��B
�BB
�vB
��B
��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B
*KB
)�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
)_B
)DB
)B
(�B
'B
!�B
�B
AB	�B	�}B
�B
l=B
�~B
��B
�B
��B
��B
��B
ÖB
ϫB
�NB
��B
��BjB<B�B$&B1vB5%BS[B��B��B��B�>B�B�B��BǮB�sB�=B�KB�B��B�sB�6B�B�B�B�B�=B�[B��B�tB�B�B�B�B�_BʦB� B�?B��B��B�MB��BvzB]~B<B�B�B%B
�DB
�B
�"B
�pB
pB
X_B
M�B
GzB
<�B
-B
*�B
 �B
B
B	�CB	��B	��B	~wB	w�B	q�B	m)B	e,B	P�B	B'B	@OB	5%B	'�B	$�B	B	hB	B�?B�B��B�B��B� BˬBʦB��B�XBȀB�+B��B�}B�B�*B�B�?B�B��B�FB��B�TB�lB��B��B|�By�BtBr-Bq�Bp�Bo�Bn}Bv`B~]B��B��B�UB��B�oB��B�B�B��B�dB��B��B��B�|B�-B��B��B�:B��B�nB��B��B�FB��B�B�"B�)B�/B��B��B�iB��B�vB�B��B�B�CB��BΥB�B	)B	
�B	�B	�B	�B	�B	5B	8lB	6zB	="B	>(B	@�B	F%B	K�B	R�B	\)B	]IB	\�B	]IB	_pB	`�B	a|B	dZB	f�B	h
B	i�B	k�B	l�B	o�B	t�B	v`B	w�B	z^B	}qB	�B	��B	��B	�YB	�EB	��B	��B	�MB	��B	��B	�mB	�UB	�IB	��B	��B	�6B	�[B	�hB	��B	�	B	�RB	�B	��B	�cB	�B	��B	��B	ƎB	�1B	��B	�B	�B	͹B	�"B	οB	�BB	�BB	��B	��B	ѝB	�4B	��B	ЗB	��B	��B	��B	�B	��B	�,B	�aB	��B	�2B	�B	՛B	�SB	׍B	�B	ٚB	ٚB	��B	��B	�7B	�#B	��B	��B	�dB	ݘB	��B	ݲB	ݲB	��B	��B	��B	�5B	ޞB	�OB	�VB	�\B	�BB	�pB	ߊB	�BB	�B	�-B	� B	�B	�fB	�RB	�B	�B	�>B	��B	�B	�B	�B	�FB	�LB	��B	�B	�_B	��B	�XB	�
B	�B	�$B	��B	��B	�B	��B	�B	�B	�IB	�B	�B	��B	�%B	��B	�LB	�2B	�RB	�8B	�lB	�	B	�XB	�XB	��B	�rB	��B	��B	��B	��B	�rB	��B	�6B	��B	�6B	��B	�wB	�HB
  B
  B
�B
�B
�B
aB
�B
�B
?B
�B
_B
1B
1B
�B
�B
�B
	�B
	�B
	�B

#B

�B
0B
�B
�B
JB
�B
^B
�B
B
B
VB
�B
B
�B
bB
bB
bB
}B
 B
�B
B
�B
�B
uB
&B
[B
�B
uB
FB
{B
�B
2B
�B
�B
B
$B
$B
�B
mB
�B
�B
mB
mB
�B
�B
�B
sB
�B
�B
KB
�B
	B
�B
�B
�B
)B
jB
�B
"NB
#TB
)DB
*�B
+�B
+kB
+B
*B
)*B
(>B
%�B
&fB
&�B
#�B
"�B
%�B
'�B
$�B
!�B
#�B
!bB
"�B
$B
#�B
$&B
$�B
)*B
,�B
.B
/�B
0oB
0�B
1[B
2|B
3�B
49B
4�B
4�B
4�B
5�B
6zB
6zB
6`B
6�B
7B
7LB
72B
72B
7�B
7�B
7�B
7�B
7�B
7�B
7�B
8B
88B
8lB
8lB
8�B
8�B
9>B
9rB
9�B
:*B
:*B
;dB
<PB
<jB
<jB
<�B
<�B
<�B
<�B
=B
<�B
<jB
;�B
:^B
8�B
7�B
7�B
8RB
8�B
8�B
8�B
9rB
;0B
:�B
:DB
:B
9�B
:�B
<B
<�B
=�B
=�B
=�B
>B
>BB
=�B
=qB
=<B
<�B
<jB
<�B
<�B
<�B
=B
=�B
=�B
=�B
=�B
>B
>BB
>wB
>�B
>�B
?B
?�B
?�B
@4B
@OB
@�B
@�B
@�B
A;B
A�B
A�B
A�B
A�B
A�B
B�B
B[B
B�B
B�B
CB
C-B
CaB
C�B
C�B
C�B
D3B
DgB
D�B
EB
E�B
E�B
E�B
E�B
F�B
F�B
GEB
GEB
GzB
G�B
H�B
IB
IRB
IRB
IlB
I�B
I�B
I�B
JrB
JXB
JrB
J�B
J�B
KB
KDB
K�B
L0B
LdB
LdB
LdB
L~B
L�B
L�B
L�B
L�B
MB
M6B
MPB
MjB
M�B
M�B
M�B
M�B
M�B
N"B
NVB
N�B
O\B
PB
P.B
PB
O�B
PbB
P}B
P�B
P�B
P�B
P�B
Q4B
Q�B
R:B
R�B
S&B
R�B
S�B
TFB
T�B
T�B
T�B
U2B
UMB
U�B
U�B
VmB
V�B
WYB
W�B
X+B
XyB
YB
Y�B
Y�B
Y�B
Y�B
Y�B
Y�B
Y�B
Y�B
Z�B
Z�B
Z�B
[�B
[�B
[�B
\CB
\CB
\xB
\xB
\�B
\�B
\�B
]IB
]�B
]�B
]�B
^B
^B
^B
^jB
^�B
^�B
_VB
_�B
_�B
_�B
`B
`BB
`\B
`vB
`�B
`�B
aB
abB
a|B
a�B
a�B
a�B
b4B
b4B
b4B
bNB
b�B
b�B
b�B
c B
c�B
c�B
c�B
c�B
c�B
dB
dZB
d�B
d�B
d�B
d�B
d�B
d�B
d�B
eB
e�B
fB
e�B
fLB
f�B
f�B
gB
g�B
g�B
g�B
hXB
hXB
h�B
h�B
h�B
iDB
jB
jKB
jKB
jeB
jB
j�B
jB
j�B
kB
k�B
k�B
k�B
lWB
lB
l"B
l"B
l"B
l"B
l=B
lqB
lqB
l�B
l�B
mB
m�B
nIB
n/B
ncB
o B
o B
oB
oB
o5B
oOB
o�B
o�B
p!B
pB
p!B
p!B
p;B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
rB
rB
raB
r|B
r|B
r�B
r�B
r�B
s3B
s3B
sMB
s�B
s�B
tB
t9B
t�B
t�B
t�B
t�B
t�B
t�B
u?B
utB
u�B
u�B
u�B
v+B
vFB
v`B
vzB
v�B
v�B
v�B
v�B
v�B
wB
wfB
w�B
w�B
xB
xlB
x�B
x�B
x�B
x�B
y$B
y	B
yXB
yXB
yrB
y�B
y�B
y�B
z*B
zDB
zxB
zxB
z�B
{B
z�B
{B
{B
{�B
{�B
{�B
{�B
{�B
|B
|jB
|jB
|�B
|�B
|�B
}B
}VB
}qB
}�B
}�B
}�B
}�B
}�B
~B
~(B
~]B
~]B
~wB
~wB
~�B
HB
�B
�B
�OB
��B
��B
��B
��B
��B
�B
� B
��B
��B
��B
��B
�'B
�AB
�[B
�[B
��B
��B
��B
�B
�B
�-B
�aB
��B
��B
��B
�B
�B
�MB
�MB
�MB
��B
��B
��B
��B
�B
�9B
��B
��B
��B
��B
��B
��B
�YB
��B
��B
��B
�+B
�_B
�_B
��B
�1B
�KB
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�	B
�XB
�XB
��B
��B
��B
��B
��B
��B
�)B
�DB
�xB
��B
��B
��B
��B
��B
�B
�JB
�JB
�dB
�dB
�~B
��B
��B
�PB
��B
��B
�"B
�B
�"B
�"B
�<B
�"B
�VB
�pB
��B
��B
�VB
��B
�BB
�vB
��B
��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20230404004941  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8b                                                                20230404004949  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20230404004949  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20230404004950                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20230404004950  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20230404004950  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20230404010516                      G�O�G�O�G�O�                