CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2016-09-30T00:35:13Z creation;2016-09-30T00:35:16Z conversion to V3.1;2019-12-19T08:29:11Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7T   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     88   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8X   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8\   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8d   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8h   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8p   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8x   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        x  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     x  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \l   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     x  `L   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     x  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     x  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �t   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     x  �T   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     x  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     x  �$   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     x  ̜   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ܤ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �$   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �(   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �,   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �0   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �4   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �t   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20160930003513  20200115111518  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               *A   JA  I2_0576_042                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @�ε|�0 1   @�ζ)�� @;$!-w2�dx|����1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�ff@�  A   A   A@  A`  A�  A�33A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C�C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C7�fC:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ�C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D.��D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX�fDY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�<�D�|�D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�3D�C3Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�<�D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�(�@�@�A�HA>�HA^�HA~�HA���A�p�A�p�A�p�A�p�A�p�A�p�A�p�B�RB�RB�RB�RB'�RB/�RB7�RB?�RBG�RBO�RBW�RB_�RBg�RBo�RBw�RB�RB��)B��)B��)B��)B��)B��)B��)B�\B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)C�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�{C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CZ�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��
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
D {�D ��D{�D��D{�D�D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D	{�D	��D
{�D
��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D {�D ��D!{�D!��D"{�D"��D#{�D#��D${�D$��D%{�D%��D&{�D&��D'{�D'��D({�D(��D){�D)��D*{�D*��D+{�D+��D,{�D,��D-{�D-��D.{�D.�D/{�D/��D0{�D0��D1{�D1��D2{�D2��D3{�D3��D4{�D4��D5{�D5��D6{�D6��D7{�D7��D8{�D8��D9{�D9��D:{�D:��D;{�D;��D<{�D<��D={�D=��D>{�D>��D?{�D?��D@{�D@��DA{�DA��DB{�DB��DC{�DC��DD{�DD��DE{�DE��DF{�DF��DG{�DG��DH{�DH��DI{�DI��DJ{�DJ��DK{�DK��DL{�DL��DM{�DM��DN{�DN��DO{�DO��DP{�DP��DQ{�DQ��DR{�DR��DS{�DS��DT{�DT��DU{�DU��DV{�DV��DW{�DW��DX��DX��DY{�DY��DZ{�DZ��D[{�D[��D\{�D\��D]{�D]��D^{�D^��D_{�D_��D`{�D`��Da{�Da��Db{�Db��Dc{�Dc��Dd{�Dd��De{�De��Df{�Df��Dg{�Dg��Dh{�Dh��Di{�Di��Dj{�Dj��Dk{�Dk��Dl{�Dl��Dm{�Dm��Dn{�Dn��Do{�Do��Dp{�Dp��Dq{�Dq��Dr{�Dr��Ds{�Ds��Dt{�Dt��Du{�Du��Dv{�Dv��Dw{�Dw��Dx{�Dx��Dy{�Dy��Dz{�Dz��D{{�D{��D|{�D|��D}{�D}��D~{�D~��D{�D��D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D� �D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D½�D���D�=�D�}�Dý�D���D�=�D�}�DĽ�D���D�=�D�}�DŽ�D���D�=�D�}�Dƽ�D���D�=�D�}�Dǽ�D���D�=�D�}�DȽ�D���D�=�D�}�Dɽ�D���D�=�D�}�Dʽ�D���D�=�D�}�D˽�D���D�=�D�}�D̽�D���D�=�D�}�Dͽ�D���D�=�D�}�Dν�D���D�=�D�}�DϽ�D���D�=�D�}�Dн�D���D�=�D�}�Dѽ�D���D�=�D�}�Dҽ�D���D�=�D�}�Dӽ�D���D�=�D�}�DԽ�D���D�:�D�z�Dս�D���D�=�D�}�Dֽ�D���D�=�D�}�D׽�D���D�=�D�}�Dؽ�D���D�=�D�}�Dٽ�D� �D�@�D�}�Dڽ�D���D�=�D�}�D۽�D���D�=�D�}�Dܽ�D���D�=�D�}�Dݽ�D���D�=�D�}�D޽�D���D�=�D�}�D߽�D���D�=�D�}�D��D���D�=�D�}�D��D���D�:�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D���D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D���D��\111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�ZA�I�A�?}A�7LA�33A�+A� �A�"�A�"�A�"�A�$�A�$�A�$�A�"�A� �A�"�A��A��A��A�VA�  A�t�A���A�hsA�A�(�A��A���A��TA�l�A�p�A��`A�33A���A�n�A��A�Q�A���A���A�VA��;A�dZA���A�VA��A��yA��`A���A���A��A�ĜA���A��A�&�A�C�A��A���A�t�A�`BA�M�A��A�G�A��;A�l�A�I�A�A��wA��A�jA�7LA�7A~�A{��Az�\Ay
=Aw�7Av��AvbNAv1Au��AuO�Au�At�RAs�Ar�jAq�AoƨAo+An�yAnĜAn�RAn��Ann�AnJAm�Al�jAk\)Aj$�AhA�Ag"�Af-AeXAd��AdA�Ac�^Ab^5A`�yA`1'A_��A_oA^A�A]�wA]x�A];dA\Q�AZffAW��AVr�AU33ATjAS��AR�AR1APr�AO��AOXAN1AL�AJ��AJbAH�AHAF�`AF�+AE�wAE�PAEl�ADVAC�wAB�\AA��A@��A@�!A@��A@�uA@ffA@(�A?C�A>-A=�A<�A;O�A:��A:  A8�!A8(�A8bA7�A6�\A5A4��A3?}A1;dA0ffA/�-A.ZA-p�A+��A*��A)��A)\)A(jA'�;A'��A'dZA&��A%�mA$�A#C�A"�A"��A"9XA!��A!`BA �yA M�A�A�AbNA1A��A�-A��AdZA��AVAXA
=A��AK�A�yAE�A�A�DAbNA9XAA|�A��A�#A�7AoA��A�!A��A��Az�AZAJA�TA��Ax�A
ffA	�A	dZA�AE�A�A�Az�A�FA�A�9AjAI�A|�A bN@�33@��/@�`B@�n�@���@�X@�  @�$�@�/@�"�@�/@���@��H@�G�@���@�ff@��@�1@�P@���@��@�o@���@۝�@��@��@�%@���@�"�@��@�b@�-@��@� �@��@�@�  @�$�@���@�A�@Ǿw@�x�@ċD@öF@ÍP@�@�~�@�v�@�1'@�J@��;@�
=@�^5@�`B@�ƨ@�E�@�Z@�+@���@���@��+@�p�@�z�@��
@���@�t�@�"�@���@���@��R@���@�v�@�5?@�X@��@�v�@�ff@�-@��#@���@��h@�&�@��u@��H@�^5@�-@��@�`B@�7L@��@�%@�1@��+@�G�@�A�@��
@��P@�
=@�E�@��@���@��@��`@���@���@�l�@��@���@�p�@���@��H@�J@���@�p�@�`B@�O�@�?}@���@���@�Q�@��@��F@�;d@�v�@�n�@��@�hs@���@��u@��D@��u@�A�@�A�@��#@�7L@�C�@��H@��y@�ȴ@��-@�p�@���@��^@��7@�O�@��@��@�&�@�?}@�?}@�/@�&�@��@�V@��j@��@�n�@��@���@���@�?}@�Ĝ@�9X@��
@�|�@�\)@�
=@��!@�v�@�V@��@���@�hs@�/@�&�@���@��`@��`@���@��j@��@�z�@�bN@�I�@�1@���@���@�@�v�@�@�G�@��9@�@|�@|�@{�F@{C�@zJ@{t�@{�@{��@|j@|��@|�j@|�/@}�@~�y@~�y@~�@~�R@~V@|�@|�D@|Z@|(�@{��@|1@{�m@{�
@{�
@{C�@z�!@z�\@z-@y�@y��@yX@y%@xbN@x  @w��@w�P@v��@v�y@v�y@v�@v��@vv�@v@u@u�-@u�h@uO�@t��@t9X@t�@s�F@r�@r�@qG�@p��@p��@pA�@o+@n��@nff@m�h@m/@m�@mO�@m��@mp�@m��@m`B@m�@mV@m�@mV@l�/@l�@lj@lZ@lZ@lZ@lZ@lj@lj@lI�@kdZ@k33@j�H@j�!@jn�@i��@i��@i�@i�#@i�#@i�#@i��@i��@i��@i�7@i�7@ix�@iX@h��@g�;@g|�@g;d@g�@g�@g�@f��@fV@e��@e�@e`B@e/@d��@d�@d�/@d�j@d�@d�D@d�D@d�D@dj@d9X@d�@c��@cƨ@cdZ@co@b�\@a�#@a�@`Ĝ@`��@`�u@`r�@`Q�@`A�@`1'@` �@`b@`b@_�@_�;@_�P@^�@^$�@]�@]�T@]��@]�-@]�@]p�@]/@\�D@[�@Z�H@Z��@Z�!@Zn�@Z=q@Z-@Z�@Y��@X��@W�@W\)@W;d@W
=@V�y@V��@Vv�@V{@U��@UO�@T��@T��@T�D@S�
@SdZ@S"�@S@R��@R�!@R�\@RM�@RJ@Q��@Q%@P�@PA�@P �@O\)@OK�@O+@N�@N�R@N��@N�+@M�@L��@L(�@K�
@Kƨ@K��@KdZ@KC�@K33@K@J��@Jn�@J�@I��@I7L@H�9@HQ�@H �@Hb@H  @H  @G��@G;d@G�@F�R@F��@FE�@E@E/@D�/@D��@Dj@Dj@Dz�@D(�@C��@B�@B�\@Bn�@BJ@A�#@A��@A��@A7L@A&�@A%@@��@@�@@Q�@@  @?�@?l�@?\)@?�@>�R@>�+@>5?@=�T@=��@=�h@=�-@=�-@=�h@=p�@<�/@<Z@<(�@;��@;dZ@:�!@:-@9�@9��@9hs@97L@8�`@8Ĝ@8Q�@7�;@7K�@6�@6E�@6@5�T@5@5�-@5`B@4��@4j@49X@3�m@3�
@3��@3"�@3o@2�H@2^5@2J@1�#@1��@1x�@1hs@1x�@1x�@1&�@0��@0�u@0  @/�;@/�@/�P@/+@.��@.ȴ@.��@.V@.V@.5?@-��@,�@,(�@+��@+�m@+ƨ@+t�@+"�@+@*�\@*�@)��@)G�@(�`@(�u@(r�@(bN@(A�@(1'@( �@'�@'�w@'��@'|�@'�@&��@&��@&@%��@%�-@%p�@%V@$�/@$�j@$��@$�D@$Z@#�F@#33@#o@"�H@"��@"n�@"-@"J@!�@!�^@!�7@!x�@!G�@!�@ �9@ A�@   @�@�w@\)@+@�@�R@��@��@�+@ff@�@@�-@p�@?}@�@��@�/@�D@j@I�@(�@��@�
@�
@ƨ@��@��@�@�@t�@t�@dZ@dZ@dZ@S�@S�@33@o@��@��@~�@n�@=q@J@��@��@�@�#@�^@�7@x�@x�@hs@G�@��@��@�@bN@Q�@1'@�@�@l�@K�@;d@+@
=@�y@�@�@��@V@�T@�-@p�@`B@`B@O�@?}@��@�@I�@1@�
@�F@��@�@dZ@C�@��@��@��@��@�@��@Ĝ@�@bN@b@  @�@��@�@|�@l�@\)@;d@
=@�@��@V@$�@�@��@��@�-@�@?}@V@�@�j@��@z�@1@��@1@1@1@�m@�
@ƨ@ƨ@�@o@
��@
�\@
n�@
-@	�7@	X@	%@��@�u@Q�@b@�w@��@|�@K�@+@+@
=@�y@�@ȴ@��@ff@E�@5?@5?@$�@{@@�@@�-@�-@��@�h@�@`B@/@��@�@�/@��@�@z�@Z@�@��@��@t�@t�@dZ@dZ111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�ZA�I�A�?}A�7LA�33A�+A� �A�"�A�"�A�"�A�$�A�$�A�$�A�"�A� �A�"�A��A��A��A�VA�  A�t�A���A�hsA�A�(�A��A���A��TA�l�A�p�A��`A�33A���A�n�A��A�Q�A���A���A�VA��;A�dZA���A�VA��A��yA��`A���A���A��A�ĜA���A��A�&�A�C�A��A���A�t�A�`BA�M�A��A�G�A��;A�l�A�I�A�A��wA��A�jA�7LA�7A~�A{��Az�\Ay
=Aw�7Av��AvbNAv1Au��AuO�Au�At�RAs�Ar�jAq�AoƨAo+An�yAnĜAn�RAn��Ann�AnJAm�Al�jAk\)Aj$�AhA�Ag"�Af-AeXAd��AdA�Ac�^Ab^5A`�yA`1'A_��A_oA^A�A]�wA]x�A];dA\Q�AZffAW��AVr�AU33ATjAS��AR�AR1APr�AO��AOXAN1AL�AJ��AJbAH�AHAF�`AF�+AE�wAE�PAEl�ADVAC�wAB�\AA��A@��A@�!A@��A@�uA@ffA@(�A?C�A>-A=�A<�A;O�A:��A:  A8�!A8(�A8bA7�A6�\A5A4��A3?}A1;dA0ffA/�-A.ZA-p�A+��A*��A)��A)\)A(jA'�;A'��A'dZA&��A%�mA$�A#C�A"�A"��A"9XA!��A!`BA �yA M�A�A�AbNA1A��A�-A��AdZA��AVAXA
=A��AK�A�yAE�A�A�DAbNA9XAA|�A��A�#A�7AoA��A�!A��A��Az�AZAJA�TA��Ax�A
ffA	�A	dZA�AE�A�A�Az�A�FA�A�9AjAI�A|�A bN@�33@��/@�`B@�n�@���@�X@�  @�$�@�/@�"�@�/@���@��H@�G�@���@�ff@��@�1@�P@���@��@�o@���@۝�@��@��@�%@���@�"�@��@�b@�-@��@� �@��@�@�  @�$�@���@�A�@Ǿw@�x�@ċD@öF@ÍP@�@�~�@�v�@�1'@�J@��;@�
=@�^5@�`B@�ƨ@�E�@�Z@�+@���@���@��+@�p�@�z�@��
@���@�t�@�"�@���@���@��R@���@�v�@�5?@�X@��@�v�@�ff@�-@��#@���@��h@�&�@��u@��H@�^5@�-@��@�`B@�7L@��@�%@�1@��+@�G�@�A�@��
@��P@�
=@�E�@��@���@��@��`@���@���@�l�@��@���@�p�@���@��H@�J@���@�p�@�`B@�O�@�?}@���@���@�Q�@��@��F@�;d@�v�@�n�@��@�hs@���@��u@��D@��u@�A�@�A�@��#@�7L@�C�@��H@��y@�ȴ@��-@�p�@���@��^@��7@�O�@��@��@�&�@�?}@�?}@�/@�&�@��@�V@��j@��@�n�@��@���@���@�?}@�Ĝ@�9X@��
@�|�@�\)@�
=@��!@�v�@�V@��@���@�hs@�/@�&�@���@��`@��`@���@��j@��@�z�@�bN@�I�@�1@���@���@�@�v�@�@�G�@��9@�@|�@|�@{�F@{C�@zJ@{t�@{�@{��@|j@|��@|�j@|�/@}�@~�y@~�y@~�@~�R@~V@|�@|�D@|Z@|(�@{��@|1@{�m@{�
@{�
@{C�@z�!@z�\@z-@y�@y��@yX@y%@xbN@x  @w��@w�P@v��@v�y@v�y@v�@v��@vv�@v@u@u�-@u�h@uO�@t��@t9X@t�@s�F@r�@r�@qG�@p��@p��@pA�@o+@n��@nff@m�h@m/@m�@mO�@m��@mp�@m��@m`B@m�@mV@m�@mV@l�/@l�@lj@lZ@lZ@lZ@lZ@lj@lj@lI�@kdZ@k33@j�H@j�!@jn�@i��@i��@i�@i�#@i�#@i�#@i��@i��@i��@i�7@i�7@ix�@iX@h��@g�;@g|�@g;d@g�@g�@g�@f��@fV@e��@e�@e`B@e/@d��@d�@d�/@d�j@d�@d�D@d�D@d�D@dj@d9X@d�@c��@cƨ@cdZ@co@b�\@a�#@a�@`Ĝ@`��@`�u@`r�@`Q�@`A�@`1'@` �@`b@`b@_�@_�;@_�P@^�@^$�@]�@]�T@]��@]�-@]�@]p�@]/@\�D@[�@Z�H@Z��@Z�!@Zn�@Z=q@Z-@Z�@Y��@X��@W�@W\)@W;d@W
=@V�y@V��@Vv�@V{@U��@UO�@T��@T��@T�D@S�
@SdZ@S"�@S@R��@R�!@R�\@RM�@RJ@Q��@Q%@P�@PA�@P �@O\)@OK�@O+@N�@N�R@N��@N�+@M�@L��@L(�@K�
@Kƨ@K��@KdZ@KC�@K33@K@J��@Jn�@J�@I��@I7L@H�9@HQ�@H �@Hb@H  @H  @G��@G;d@G�@F�R@F��@FE�@E@E/@D�/@D��@Dj@Dj@Dz�@D(�@C��@B�@B�\@Bn�@BJ@A�#@A��@A��@A7L@A&�@A%@@��@@�@@Q�@@  @?�@?l�@?\)@?�@>�R@>�+@>5?@=�T@=��@=�h@=�-@=�-@=�h@=p�@<�/@<Z@<(�@;��@;dZ@:�!@:-@9�@9��@9hs@97L@8�`@8Ĝ@8Q�@7�;@7K�@6�@6E�@6@5�T@5@5�-@5`B@4��@4j@49X@3�m@3�
@3��@3"�@3o@2�H@2^5@2J@1�#@1��@1x�@1hs@1x�@1x�@1&�@0��@0�u@0  @/�;@/�@/�P@/+@.��@.ȴ@.��@.V@.V@.5?@-��@,�@,(�@+��@+�m@+ƨ@+t�@+"�@+@*�\@*�@)��@)G�@(�`@(�u@(r�@(bN@(A�@(1'@( �@'�@'�w@'��@'|�@'�@&��@&��@&@%��@%�-@%p�@%V@$�/@$�j@$��@$�D@$Z@#�F@#33@#o@"�H@"��@"n�@"-@"J@!�@!�^@!�7@!x�@!G�@!�@ �9@ A�@   @�@�w@\)@+@�@�R@��@��@�+@ff@�@@�-@p�@?}@�@��@�/@�D@j@I�@(�@��@�
@�
@ƨ@��@��@�@�@t�@t�@dZ@dZ@dZ@S�@S�@33@o@��@��@~�@n�@=q@J@��@��@�@�#@�^@�7@x�@x�@hs@G�@��@��@�@bN@Q�@1'@�@�@l�@K�@;d@+@
=@�y@�@�@��@V@�T@�-@p�@`B@`B@O�@?}@��@�@I�@1@�
@�F@��@�@dZ@C�@��@��@��@��@�@��@Ĝ@�@bN@b@  @�@��@�@|�@l�@\)@;d@
=@�@��@V@$�@�@��@��@�-@�@?}@V@�@�j@��@z�@1@��@1@1@1@�m@�
@ƨ@ƨ@�@o@
��@
�\@
n�@
-@	�7@	X@	%@��@�u@Q�@b@�w@��@|�@K�@+@+@
=@�y@�@ȴ@��@ff@E�@5?@5?@$�@{@@�@@�-@�-@��@�h@�@`B@/@��@�@�/@��@�@z�@Z@�@��@��@t�@t�@dZ@dZ111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�=B�=B�7B�1B�+B�+B�%B�%B�%B�%B�%B�%B�%B�%B�%B�%B�%B�B�B�B� Bl�BL�B@�B7LB�BB��B�B�B�B�}B�B��B�DBq�BbNB?}B)�B�B��B�dB��B�DB}�Bq�BT�B>wB,B �B�B�BoB
=B
�sB
ĜB
�qB
�dB
�XB
�LB
�3B
��B
��B
�\B
�VB
�DB
�DB
�DB
�DB
�1B
�%B
}�B
v�B
l�B
gmB
^5B
ZB
XB
W
B
VB
T�B
R�B
P�B
L�B
F�B
>wB
5?B
0!B
.B
-B
,B
,B
)�B
&�B
$�B
�B
�B
JB
B	��B	�B	�B	�fB	�NB	�5B	�B	��B	ɺB	ŢB	ÖB	�jB	�XB	�FB	�9B	�B	��B	�uB	�DB	�B	� B	|�B	v�B	q�B	iyB	e`B	dZB	`BB	VB	O�B	L�B	F�B	D�B	?}B	>wB	;dB	:^B	<jB	8RB	2-B	,B	$�B	�B	�B	�B	�B	�B	�B	{B	\B		7B	1B	B	  B	B	B��B��B��B��B�B�sB�;B��B��BȴB��B�dB�?B�-B�!B�B�B��B��B��B��B��B��B��B��B��B�{B�oB�bB�\B�PB�=B�1B�%B�B�B�B�B�B�B{�Bx�Bs�Bk�BhsBffBe`Be`BbNBaHB`BB`BB_;B\)BZBYBXBW
BVBT�BR�BQ�BP�BP�BO�BN�BO�BL�BI�BI�BG�BE�BE�BD�B=qB;dB:^B8RB7LB7LB5?B33B1'B1'B,B+B'�B&�B&�B$�B"�B!�B �B�B�B�B�B�B�B�B�B�B�BoBoBbB\B\B\B\BVBJB\BPBPBPBVB\B\BhBbB\B\BuBhBoBhBoBoBbB�B{B�B�B�B�B�B�B!�B!�B!�B"�B#�B&�B'�B'�B'�B'�B(�B(�B(�B(�B(�B(�B'�B+B0!B1'B0!B1'B2-B2-B2-B33B5?B=qB=qB>wB>wB?}B?}B?}B?}BA�BB�BE�BI�BJ�BM�BQ�BR�BW
B]/BaHBe`BgmBk�Bk�BjBk�Bl�BjBl�Bo�Bq�Bs�Bt�Bt�Bt�Bv�Bw�B}�B}�B}�B�B�B�%B�=B�VB�\B�hB�uB�{B��B��B��B��B��B��B�B�-B�3B�LB��B��BBŢB��B��B�#B�)B�5B�;B�HB�ZB�B�B�B�B�B�B�B��B��B��B��B��B��B	B	B	+B		7B	DB	JB	VB	oB	�B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	"�B	$�B	&�B	'�B	%�B	$�B	#�B	$�B	&�B	&�B	'�B	&�B	)�B	,B	5?B	;dB	A�B	D�B	E�B	G�B	H�B	K�B	W
B	YB	ZB	[#B	]/B	_;B	`BB	`BB	bNB	dZB	ffB	k�B	l�B	m�B	o�B	s�B	u�B	x�B	z�B	{�B	}�B	� B	�B	�B	�1B	�7B	�DB	�DB	�DB	�DB	�JB	�PB	�VB	�\B	�\B	�bB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�'B	�-B	�3B	�9B	�?B	�FB	�^B	�jB	�jB	�jB	�jB	�jB	�qB	�}B	B	ĜB	ƨB	ǮB	ǮB	ɺB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�B	�B	�#B	�#B	�)B	�)B	�)B	�)B	�/B	�/B	�/B	�5B	�;B	�BB	�HB	�TB	�`B	�fB	�fB	�fB	�fB	�mB	�mB	�mB	�mB	�mB	�sB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
B
B
B
B
B
%B
+B
1B
	7B
	7B
	7B
DB
DB
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
\B
\B
bB
bB
bB
bB
bB
hB
oB
oB
oB
uB
uB
uB
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
�B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
"�B
#�B
#�B
#�B
$�B
$�B
$�B
%�B
%�B
%�B
%�B
&�B
(�B
)�B
)�B
+B
+B
,B
,B
-B
-B
.B
.B
/B
/B
0!B
0!B
1'B
2-B
2-B
2-B
2-B
33B
2-B
33B
33B
33B
33B
49B
33B
33B
49B
49B
49B
5?B
6FB
6FB
7LB
7LB
7LB
7LB
7LB
7LB
7LB
9XB
:^B
:^B
;dB
;dB
<jB
<jB
;dB
;dB
;dB
<jB
<jB
=qB
=qB
=qB
=qB
=qB
=qB
>wB
>wB
>wB
>wB
?}B
?}B
@�B
@�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
D�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
F�B
G�B
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
I�B
J�B
J�B
J�B
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
L�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
O�B
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
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
T�B
VB
VB
VB
VB
VB
W
B
W
B
W
B
XB
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
^5B
^5B
^5B
_;B
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
aHB
aHB
aHB
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
jB
jB
jB
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
m�B
m�B
m�B
m�B
m�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�rB�rB�RB�KB�EB�EB�%B�%B�%B�?B�%B�%B�%B�%B�%B�?B�?B�mB�B��B��Bw�BU�BLdBB�B!B3B��B�nB�aBݘB�B��B�`B�BxBh>BEmB3�B��BյB�.B��B�jB��Bu�BW�B@�B-CB!|B=BmB�B�B
�B
ňB
��B
��B
��B
��B
�%B
��B
��B
��B
��B
��B
��B
��B
�0B
��B
�B
��B
xRB
n}B
iB
_VB
Z�B
X�B
W�B
V�B
UgB
S�B
RB
NVB
HfB
@4B
6B
0oB
.IB
-)B
,=B
,�B
*�B
'�B
&2B
�B
_B
pB
uB	��B	��B	�WB	�8B	�TB	��B	ٚB	οB	ʦB	�tB	āB	�"B	��B	��B	��B	��B	��B	�B	��B	�9B	� B	~(B	w�B	shB	j�B	fLB	f2B	b�B	WsB	Q4B	N<B	G�B	E�B	@4B	?cB	;�B	:�B	=�B	9XB	3�B	-)B	%�B	B	�B	�B	B	KB	�B	B	�B	
=B		�B	3B	;B	uB	�B�PB��B�PB�B�oB�B�|B�MB�BʌB�B�qB��B�B�B�)B��B��B��B��B��B��B��B�KB�
B�?B�B�&B�4B�HB�<B�DB��B��B��B�gB��B��B�3B�-B}�B{�Bu%Bl=BiDBg�Bf�BfBb�Ba�B`�Ba-B`\B]IBZ�BY�BX�BW�BWYBVBSuBRTBQ�BQNBPHBO�BQNBM�BJ�BJ�BH�BF�BHBF�B>�B<6B:�B8�B8B8�B6�B4�B3B3hB-�B+�B(�B($B($B%�B$@B# B!�B�B�BB7BeB1B�BYB�B?B�B@B�BBB.B�B�BB�B<B"B<B\B�B}B B�B�B�B,BB�B�B�BB:B
BBKBKBB�B�BB"�B"B"NB# B$�B'�B(sB(XB($B(>B)DB)B)B)DB)DB)_B(�B,qB0�B1[B0�B1vB2|B2|B2�B4B6FB=�B=�B>�B>�B?�B?�B@ B@iBB�BC{BFtBJ	BKDBNVBR�BS�BWsB]IBa|Be�Bh$Bk�Bk�BkBl�Bm�Bk6Bm)BpBq�Bs�Bt�Bt�BuBv�Bx8B~]B~BB~wB��B�SB��B��B��B��B��B��B��B��B�B��B��B�2B�
B�cB��B�hB�LB��B��B��BżB�B��B�	B�]B�OB�VB�bB�B��B�IB�B�B�B��B�B�%B�2B�$B�DB�B�<B	oB	mB	zB		�B	xB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	!B	# B	%FB	'�B	(XB	&fB	%�B	$ZB	%�B	'�B	'B	(>B	'8B	*0B	+�B	5%B	;JB	AoB	D�B	E�B	G�B	H�B	K�B	W
B	YKB	Z7B	[�B	]�B	_�B	`\B	`\B	bhB	dZB	f�B	k�B	l�B	m�B	o�B	s�B	vB	x�B	z�B	|B	~(B	�OB	�aB	�SB	�KB	��B	�^B	�^B	�^B	�^B	�~B	��B	�pB	�vB	�vB	��B	��B	��B	��B	��B	�B	�!B	�:B	� B	��B	�,B	�8B	�B	�&B	�&B	�B	��B	��B	�B	�"B	�"B	�IB	�UB	�AB	�GB	�hB	�nB	�ZB	�zB	�xB	�jB	�jB	��B	�jB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�\B	�B	�B	�B	�B	�B	�B	�MB	�SB	�$B	�EB	�1B	�1B	�7B	�7B	�7B	�=B	�=B	�)B	�)B	�CB	�CB	�dB	�dB	�dB	�jB	�pB	�vB	�B	�B	�B	�B	�B	�B	�B	�mB	�B	�B	�mB	�mB	�B	�B	�B	��B	��B	��B	�B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�+B	�>B	�B	��B	�B	�B	�B	�B	�"B	�B	�(B
 B
 4B
 OB
UB
-B
GB
3B
3B
SB
SB
9B
SB
YB
_B
fB
	lB
	RB
	�B
^B
xB
^B
dB
dB
~B
�B
�B
�B
�B
vB
vB
�B
vB
}B
�B
}B
}B
�B
�B
�B
�B
�B
�B
uB
�B
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
�B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
#B
#�B
#�B
#�B
$�B
$�B
$�B
%�B
%�B
%�B
&B
'8B
)*B
*B
*B
+QB
+QB
,WB
,=B
-)B
-)B
./B
./B
/5B
/OB
0UB
0UB
1[B
2aB
2GB
2GB
2GB
3MB
2aB
3hB
3hB
3hB
3hB
4TB
3hB
3hB
4TB
4TB
4nB
5tB
6`B
6zB
7�B
7LB
7LB
7�B
7fB
7fB
7�B
9�B
:�B
:xB
;�B
;�B
<�B
<�B
;B
;�B
;�B
<�B
<�B
=�B
=�B
=�B
=�B
=�B
=�B
>�B
>�B
>�B
>�B
?�B
?�B
@�B
@�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
D�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
F�B
G�B
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
I�B
J�B
KB
KB
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
MB
MB
NB
NB
M�B
N�B
N�B
OB
N�B
N�B
O�B
PB
PB
O�B
O�B
O�B
O�B
PB
O�B
P�B
Q B
P�B
P�B
Q B
P�B
P�B
Q B
P�B
Q B
Q B
QB
Q B
RB
RB
RB
R B
RB
Q�B
Q�B
RB
R B
S&B
R�B
SB
S&B
SB
SB
SB
SB
TB
T,B
TB
TB
TB
U2B
UB
UB
T�B
U2B
VB
VB
VB
V9B
VB
W?B
W?B
W?B
XB
X+B
X+B
X+B
XEB
X+B
X_B
YKB
YKB
YKB
Z7B
Z7B
Z7B
ZQB
ZQB
[�B
\]B
\]B
\xB
]IB
]dB
]IB
]IB
]IB
^OB
^OB
^jB
^OB
^OB
_;B
_VB
_VB
_pB
_pB
_pB
_VB
`\B
`\B
`\B
`BB
`\B
`\B
abB
abB
abB
bhB
bhB
b�B
b�B
cnB
cTB
cTB
cTB
cnB
cTB
cTB
cnB
c�B
c�B
dtB
dtB
dtB
d�B
e�B
ezB
ezB
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
h�B
hsB
h�B
i�B
i�B
i�B
iyB
i�B
iyB
iyB
j�B
jB
j�B
jB
j�B
j�B
jB
j�B
j�B
j�B
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
m�B
m�B
m�B
m�B
m�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<49X<#�
<>�<49X<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.07(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201610040035352016100400353520161004003535201806221214402018062212144020180622121440201804050407242018040504072420180405040724  JA  ARFMdecpA19c                                                                20160930093505  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20160930003513  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20160930003514  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20160930003514  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20160930003515  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20160930003515  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20160930003515  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20160930003515  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20160930003515  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20160930003516                      G�O�G�O�G�O�                JA  ARUP                                                                        20160930012149                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20160930153235  CV  JULD            G�O�G�O�F�u�                JM  ARGQJMQC2.0                                                                 20160930153235  CV  JULD_LOCATION   G�O�G�O�F�u�                JM  ARGQJMQC2.0                                                                 20160930153235  CV  LATITUDE        G�O�G�O�A��                JM  ARGQJMQC2.0                                                                 20160930153235  CV  LONGITUDE       G�O�G�O��#�Z                JM  ARCAJMQC2.0                                                                 20161003153535  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20161003153535  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404190724  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622031440  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115111518                      G�O�G�O�G�O�                