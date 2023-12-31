CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-04-28T21:35:19Z creation;2018-04-28T21:35:25Z conversion to V3.1;2019-12-19T07:43:37Z update;     
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
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I<   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M$   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  pL   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t4   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �\   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �D   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �l   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ݬ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �<   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �<   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �<   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �<   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �    HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �0   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �4   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �8   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �<Argo profile    3.1 1.2 19500101000000  20180428213519  20200115131517  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0576_234                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @�^�w`�1   @�^�>�� @:BZ�c �de�i�B�1   GPS     A   B   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�ff@�33A   A   A>ffA^ffA�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C�fC  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D�fDfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�C3Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�<�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�(�@���@�A�HA=G�A]G�A~�HA�p�A�p�A�p�A�p�A�p�A�p�A�p�A�p�B�RB�RB�RB�RB'�RB/�RB7�RB?�RBG�RBO�RBW�RB_�RBg�RBo�RBw�RB�RB��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)C�C�C�C�C	�C�C�C�C�C�{C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
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
��D{�D��D{�D��D{�D��D{�D��D��D�D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D {�D ��D!{�D!��D"{�D"��D#{�D#��D${�D$��D%{�D%��D&{�D&��D'{�D'��D({�D(��D){�D)��D*{�D*��D+{�D+��D,{�D,��D-{�D-��D.{�D.��D/{�D/��D0{�D0��D1{�D1��D2{�D2��D3{�D3��D4{�D4��D5{�D5��D6{�D6��D7{�D7��D8{�D8��D9{�D9��D:{�D:��D;{�D;��D<{�D<��D={�D=��D>{�D>��D?{�D?��D@{�D@��DA{�DA��DB{�DB��DC{�DC��DD{�DD��DE{�DE��DF{�DF��DG{�DG��DH{�DH��DI{�DI��DJ{�DJ��DK{�DK��DL{�DL��DM{�DM��DN{�DN��DO{�DO��DP{�DP��DQ{�DQ��DR{�DR��DS{�DS��DT{�DT��DU{�DU��DV{�DV��DW{�DW��DX{�DX��DY{�DY��DZ{�DZ��D[{�D[��D\{�D\��D]{�D]��D^{�D^��D_{�D_��D`{�D`��Da{�Da��Db{�Db��Dc{�Dc��Dd{�Dd��De{�De��Df{�Df��Dg{�Dg��Dh{�Dh��Di{�Di��Dj{�Dj��Dk{�Dk��Dl{�Dl��Dm{�Dm��Dn{�Dn��Do{�Do��Dp{�Dp��Dq{�Dq��Dr{�Dr��Ds{�Ds��Dt{�Dt��Du{�Du��Dv{�Dv��Dw{�Dw��Dx{�Dx��Dy{�Dy��Dz{�Dz��D{{�D{��D|{�D|��D}{�D}��D~{�D~��D{�D��D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�@�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�z�D���D���D�=�D�}�D���D���D�=�D�}�D���D� �D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D½�D���D�=�D�}�Dý�D���D�=�D�}�DĽ�D���D�=�D�}�DŽ�D���D�=�D�}�Dƽ�D���D�=�D�}�Dǽ�D���D�=�D�}�DȽ�D���D�=�D�}�Dɽ�D���D�=�D�}�Dʽ�D���D�=�D�}�D˽�D���D�=�D�}�D̽�D���D�=�D�}�Dͽ�D���D�=�D�}�Dν�D���D�=�D�}�DϽ�D���D�=�D�}�Dн�D���D�=�D�}�Dѽ�D���D�=�D�}�Dҽ�D���D�=�D�}�Dӽ�D���D�=�D�}�DԽ�D���D�=�D�}�Dս�D���D�=�D�}�Dֽ�D���D�=�D�}�D׽�D���D�=�D�}�Dؽ�D���D�=�D�}�Dٽ�D���D�@�D�}�Dڽ�D���D�=�D�}�D۽�D���D�=�D�}�Dܽ�D���D�=�D�}�Dݽ�D���D�=�D�}�D޽�D���D�=�D�}�D߽�D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D���D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D� �D�:�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A���A��A��RA��jA��jA��jA��wA��jA��^A��^A��RA���A���A�A���A��\A��wA�^5A�C�A��`A�$�A��;A���A� �A��
A���A�ĜA��7A�ffA�A�dZA��A�XA��+A�z�A�JA��
A�bA�ĜA�/A�ƨA�7LA���A�(�A�z�A��!A�I�A�?}A�
=A��wA�dZA���A�7LA�JA��^A��A���A�"�A�ZA��+A��A��#A�`BA���A���A��A���A�
=A�%A��/A�A�A�|�A�/A�{A�^5A�%A��\A��A��A~~�A|��A{��AzJAx$�Av(�At�ArZAq\)Ap��Ap{AoO�AnbAl�/Al-AkAk�Aj��Ai��AhĜAh5?Ag�FAf�AeG�Ad  Ac�PAcAb�Aa�Aal�AaoA`5?A_K�A^��A]�A[K�AZ(�AYK�AX�HAXr�AXI�AW�mAV~�ATJAQ�ANA�AMC�AM�ALv�AK�wAK�AKS�AKVAJM�AH�DAF��AFbNAE�TAD�yAB�A@�`A@I�A@$�A?C�A>�\A=��A=\)A<�A;�FA;hsA:�A9��A9?}A7G�A6�A6�HA6�9A5��A5+A5
=A4�A4ZA3K�A0�!A/�TA/hsA.��A.VA,�9A*�9A)��A(��A(M�A'S�A%�A%hsA%;dA%33A%&�A$��A#�A!�A!�7A 5?A�AA�\Ax�Ar�A��Ax�AS�A7LAoA��A��A�A(�A��A"�A$�AQ�A�AhsA\)AK�A33AA�A�+AhsAv�A��AQ�A�PA
ȴA
I�A	A��AVA\)AA��AAl�A��AI�AA�-A7LA��AJA7LA �\A   @��@���@�hs@�(�@��@�\)@��@���@�7L@�ff@���@��m@�@�p�@�7L@���@�@�9X@�\)@�~�@�u@��H@�(�@�;d@�ff@�7@�j@�!@�V@�I�@��m@ߍP@�t�@�v�@��@ܼj@�9X@ڰ!@أ�@���@�@��#@�I�@ӥ�@�dZ@���@�-@���@�r�@�Q�@� �@���@ϥ�@�t�@�\)@��y@�?}@̣�@�t�@�V@��T@�&�@���@��m@őh@þw@��H@�@�ƨ@���@���@�9X@��@��m@�ƨ@�l�@�n�@��j@��@��
@�l�@�o@��R@�X@�  @���@�K�@��H@�@�I�@�|�@���@�V@�@�7L@��9@��@���@��@�j@�\)@��\@�~�@�=q@���@�p�@�G�@��@�r�@��@���@�@��@�E�@���@���@��!@�=q@�@���@�@���@��@�`B@��@���@��u@�Q�@��w@�o@�?}@���@��j@���@���@��u@�r�@�(�@��m@��F@�|�@�C�@�o@���@�-@�x�@�j@���@�;d@���@�M�@�-@��@���@�V@�r�@��w@���@�C�@�33@�
=@�~�@�{@���@��@��m@���@�|�@�dZ@�33@���@��@�?}@�%@��/@��D@�(�@�S�@�o@�@��H@���@��!@���@���@���@��\@���@���@���@��\@�n�@�=q@��@�@��7@��@��`@��9@���@���@��u@��D@�r�@�j@�bN@�I�@� �@�b@���@���@�S�@���@���@�~�@�E�@�-@��@���@���@�hs@�G�@�&�@��@�%@���@��/@��@�r�@�9X@�b@�  @�;@��@\)@~��@~��@}�T@|�D@|I�@|(�@{��@zJ@yX@x��@x�@xr�@xr�@xr�@xbN@xbN@xQ�@xQ�@xA�@w�;@w\)@w;d@w�@w
=@v�R@vff@vE�@u@u��@u��@uO�@sdZ@r~�@q�#@qx�@qX@q�7@q�#@q�#@q%@o\)@n��@n5?@n@m@m��@mO�@l��@lI�@k��@k��@k��@k��@k�m@k�
@k�
@k�F@k33@ko@j�H@j��@j�!@j��@j��@j=q@jJ@i��@i��@iG�@i�@hĜ@h1'@g;d@g
=@g+@g+@g;d@f�y@f5?@e�T@e@e�@e`B@e?}@eV@d��@d�@d�@c�
@c"�@b~�@a�#@a��@a��@aG�@a�@`��@`��@`b@_�w@_��@_l�@_;d@_�@^�y@^�+@^{@]?}@]V@\��@\��@]V@\�@\��@\�j@\j@\(�@[��@[�
@[o@Z^5@Z-@Z�@ZJ@Y��@Y%@X�@W��@WK�@W;d@W+@W
=@V�y@V�@Vȴ@V5?@U�-@U/@T��@T1@S��@R�@R�\@Q��@Qhs@Q�@P�9@P��@P�@PA�@P1'@Pb@Pb@O�;@O��@O\)@O
=@N5?@M@Mp�@M�@L�/@L�@L�D@LI�@K�m@KdZ@K"�@K@J��@Jn�@I��@I�^@Ihs@H��@H�9@HA�@H  @G�w@G|�@GK�@G�@F��@Fv�@Fv�@F$�@E@E��@E�h@E�@E?}@D�/@D��@D�@Dj@C�F@CC�@C"�@Co@B�@B��@A�#@AG�@@�9@@r�@@Q�@@b@?�w@>��@>V@>5?@=��@<�@<�j@<z�@<�@;�m@;�F@;��@;�@;33@:��@:^5@9�@9�^@9��@9x�@9G�@97L@9&�@8�9@8�u@8r�@8  @7|�@6�@6��@6ff@6V@6E�@6$�@5��@5O�@4��@4�D@4�@41@3��@3�
@3ƨ@3dZ@2�@2��@2^5@2�@1��@1��@1X@1�@1%@0��@0��@0A�@0 �@/�@/l�@/K�@/;d@.�@-�@-?}@,��@,��@,�@,�@,��@,�D@,j@,Z@,�@,1@+��@+�m@+��@+�@+t�@+t�@+@*�\@*�@)��@)��@)�7@)x�@)X@)�@(��@(��@(�u@(r�@(Q�@(  @'�@'�;@'�w@'\)@'+@&��@&��@&��@&v�@&v�@&V@&$�@&{@%�T@%`B@%�@$��@$��@$9X@#ƨ@#�@#C�@#33@#o@"�@"��@"�\@"~�@"~�@"n�@"-@!��@!7L@ ��@ bN@ 1'@  �@ b@   @|�@�@�y@ȴ@ȴ@��@�+@V@V@5?@�h@�/@��@j@9X@�m@S�@o@o@�@��@��@^5@=q@-@J@�#@x�@7L@&�@�@%@��@�`@��@��@�u@�u@�@�@�@�@bN@ �@�@|�@l�@\)@+@�@ȴ@��@�+@5?@�@�T@��@?}@/@�@��@�@��@z�@�@ƨ@��@dZ@"�@o@�@�H@��@��@=q@��@��@��@��@��@��@��@��@��@x�@G�@�`@r�@Q�@A�@  @\)@�@��@�@��@�+@ff@V@E�@E�@5?@$�@5?@$�@{@�T@�@?}@V@�/@�j@�D@(�@t�@o@
�@
��@
��@
��@
��@
��@
~�@
�@	��@	��@	��@	7L@Ĝ@�u@�u@�@�@r�@Q�@A�@b@��@�@\)@��@�y@�@�@ȴ@v�@5?@@@�@�@�T@@��@��@�h@`B@/@�@V@�/@z�@(�@�@(�@(�@9X@(�@1@�
@�F@��@�@dZ@dZ@C�@"�@o@@@�@�@��@�\@�\@�\@n�@�@��@��1111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A���A��A��RA��jA��jA��jA��wA��jA��^A��^A��RA���A���A�A���A��\A��wG�O�G�O�A��`A�$�A��;A���A� �A��
A���A�ĜA��7A�ffA�A�dZA��A�XA��+A�z�A�JA��
A�bA�ĜA�/A�ƨA�7LA���A�(�A�z�A��!A�I�A�?}A�
=A��wA�dZA���A�7LA�JA��^A��A���A�"�A�ZA��+A��A��#A�`BA���A���A��A���A�
=A�%A��/A�A�A�|�A�/A�{A�^5A�%A��\A��A��A~~�A|��A{��AzJAx$�Av(�At�ArZAq\)Ap��Ap{AoO�AnbAl�/Al-AkAk�Aj��Ai��AhĜAh5?Ag�FAf�AeG�Ad  Ac�PAcAb�Aa�Aal�AaoA`5?A_K�A^��A]�A[K�AZ(�AYK�AX�HAXr�AXI�AW�mAV~�ATJAQ�ANA�AMC�AM�ALv�AK�wAK�AKS�AKVAJM�AH�DAF��AFbNAE�TAD�yAB�A@�`A@I�A@$�A?C�A>�\A=��A=\)A<�A;�FA;hsA:�A9��A9?}A7G�A6�A6�HA6�9A5��A5+A5
=A4�A4ZA3K�A0�!A/�TA/hsA.��A.VA,�9A*�9A)��A(��A(M�A'S�A%�A%hsA%;dA%33A%&�A$��A#�A!�A!�7A 5?A�AA�\Ax�Ar�A��Ax�AS�A7LAoA��A��A�A(�A��A"�A$�AQ�A�AhsA\)AK�A33AA�A�+AhsAv�A��AQ�A�PA
ȴA
I�A	A��AVA\)AA��AAl�A��AI�AA�-A7LA��AJA7LA �\A   @��@���@�hs@�(�@��@�\)@��@���@�7L@�ff@���@��m@�@�p�@�7L@���@�@�9X@�\)@�~�@�u@��H@�(�@�;d@�ff@�7@�j@�!@�V@�I�@��m@ߍP@�t�@�v�@��@ܼj@�9X@ڰ!@أ�@���@�@��#@�I�@ӥ�@�dZ@���@�-@���@�r�@�Q�@� �@���@ϥ�@�t�@�\)@��y@�?}@̣�@�t�@�V@��T@�&�@���@��m@őh@þw@��H@�@�ƨ@���@���@�9X@��@��m@�ƨ@�l�@�n�@��j@��@��
@�l�@�o@��R@�X@�  @���@�K�@��H@�@�I�@�|�@���@�V@�@�7L@��9@��@���@��@�j@�\)@��\@�~�@�=q@���@�p�@�G�@��@�r�@��@���@�@��@�E�@���@���@��!@�=q@�@���@�@���@��@�`B@��@���@��u@�Q�@��w@�o@�?}@���@��j@���@���@��u@�r�@�(�@��m@��F@�|�@�C�@�o@���@�-@�x�@�j@���@�;d@���@�M�@�-@��@���@�V@�r�@��w@���@�C�@�33@�
=@�~�@�{@���@��@��m@���@�|�@�dZ@�33@���@��@�?}@�%@��/@��D@�(�@�S�@�o@�@��H@���@��!@���@���@���@��\@���@���@���@��\@�n�@�=q@��@�@��7@��@��`@��9@���@���@��u@��D@�r�@�j@�bN@�I�@� �@�b@���@���@�S�@���@���@�~�@�E�@�-@��@���@���@�hs@�G�@�&�@��@�%@���@��/@��@�r�@�9X@�b@�  @�;@��@\)@~��@~��@}�T@|�D@|I�@|(�@{��@zJ@yX@x��@x�@xr�@xr�@xr�@xbN@xbN@xQ�@xQ�@xA�@w�;@w\)@w;d@w�@w
=@v�R@vff@vE�@u@u��@u��@uO�@sdZ@r~�@q�#@qx�@qX@q�7@q�#@q�#@q%@o\)@n��@n5?@n@m@m��@mO�@l��@lI�@k��@k��@k��@k��@k�m@k�
@k�
@k�F@k33@ko@j�H@j��@j�!@j��@j��@j=q@jJ@i��@i��@iG�@i�@hĜ@h1'@g;d@g
=@g+@g+@g;d@f�y@f5?@e�T@e@e�@e`B@e?}@eV@d��@d�@d�@c�
@c"�@b~�@a�#@a��@a��@aG�@a�@`��@`��@`b@_�w@_��@_l�@_;d@_�@^�y@^�+@^{@]?}@]V@\��@\��@]V@\�@\��@\�j@\j@\(�@[��@[�
@[o@Z^5@Z-@Z�@ZJ@Y��@Y%@X�@W��@WK�@W;d@W+@W
=@V�y@V�@Vȴ@V5?@U�-@U/@T��@T1@S��@R�@R�\@Q��@Qhs@Q�@P�9@P��@P�@PA�@P1'@Pb@Pb@O�;@O��@O\)@O
=@N5?@M@Mp�@M�@L�/@L�@L�D@LI�@K�m@KdZ@K"�@K@J��@Jn�@I��@I�^@Ihs@H��@H�9@HA�@H  @G�w@G|�@GK�@G�@F��@Fv�@Fv�@F$�@E@E��@E�h@E�@E?}@D�/@D��@D�@Dj@C�F@CC�@C"�@Co@B�@B��@A�#@AG�@@�9@@r�@@Q�@@b@?�w@>��@>V@>5?@=��@<�@<�j@<z�@<�@;�m@;�F@;��@;�@;33@:��@:^5@9�@9�^@9��@9x�@9G�@97L@9&�@8�9@8�u@8r�@8  @7|�@6�@6��@6ff@6V@6E�@6$�@5��@5O�@4��@4�D@4�@41@3��@3�
@3ƨ@3dZ@2�@2��@2^5@2�@1��@1��@1X@1�@1%@0��@0��@0A�@0 �@/�@/l�@/K�@/;d@.�@-�@-?}@,��@,��@,�@,�@,��@,�D@,j@,Z@,�@,1@+��@+�m@+��@+�@+t�@+t�@+@*�\@*�@)��@)��@)�7@)x�@)X@)�@(��@(��@(�u@(r�@(Q�@(  @'�@'�;@'�w@'\)@'+@&��@&��@&��@&v�@&v�@&V@&$�@&{@%�T@%`B@%�@$��@$��@$9X@#ƨ@#�@#C�@#33@#o@"�@"��@"�\@"~�@"~�@"n�@"-@!��@!7L@ ��@ bN@ 1'@  �@ b@   @|�@�@�y@ȴ@ȴ@��@�+@V@V@5?@�h@�/@��@j@9X@�m@S�@o@o@�@��@��@^5@=q@-@J@�#@x�@7L@&�@�@%@��@�`@��@��@�u@�u@�@�@�@�@bN@ �@�@|�@l�@\)@+@�@ȴ@��@�+@5?@�@�T@��@?}@/@�@��@�@��@z�@�@ƨ@��@dZ@"�@o@�@�H@��@��@=q@��@��@��@��@��@��@��@��@��@x�@G�@�`@r�@Q�@A�@  @\)@�@��@�@��@�+@ff@V@E�@E�@5?@$�@5?@$�@{@�T@�@?}@V@�/@�j@�D@(�@t�@o@
�@
��@
��@
��@
��@
��@
~�@
�@	��@	��@	��@	7L@Ĝ@�u@�u@�@�@r�@Q�@A�@b@��@�@\)@��@�y@�@�@ȴ@v�@5?@@@�@�@�T@@��@��@�h@`B@/@�@V@�/@z�@(�@�@(�@(�@9X@(�@1@�
@�F@��@�@dZ@dZ@C�@"�@o@@@�@�@��@�\@�\@�\@n�@�@��@��1111111111111111144111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�yB�yB�sB�mB�mB�mB�mB�mB�mB�mB�mB�fB�ZB�HB�#B�/BƨB�jB�PBw�B�RB�LB�B��B��B�{B��B��B�DB�VB�7Bz�Bl�B|�BiyBH�BN�BR�BD�BA�B6FB)�B(�BuBhBbB�BVB��B�BĜB�jB�!B��B��B��B�bB� BjBdZBO�BT�BA�B$�B	7BB
��B
�ZB
ǮB
��B
��B
ŢB
B
��B
}�B
_;B
p�B
O�B
@�B
<jB
=qB
0!B
�B

=B
1B	��B	��B
%B	��B	�B	�mB	�5B	�B	�yB	�ZB	�)B	��B	��B	��B	ĜB	�?B	�'B	��B	�3B	�B	�B	��B	��B	��B	��B	�PB	�=B	v�B	k�B	q�B	x�B	y�B	v�B	s�B	dZB	G�B	�B	�B	B	)�B	=qB	2-B	-B	2-B	/B	$�B	hB��B�B	DB	B�B�B�)B�B�B�ZB�5B�5B�BȴB��B��BȴB�FB�}B��B�^BB�qB�!B�!B�LB�!B��B�DBl�B�DB�PB�7B{�BffBYBhsBq�Bn�BffB_;Bm�Bv�Bv�Bq�BffBO�B8RBYBH�B?}BB�B;dBE�BL�BW
BbNBe`BffBe`BdZB`BB]/BXBN�BN�B?}B/BK�BZBZBT�BL�B9XB49B;dB.B-B#�B1'B49B1'B8RB7LB-B6FB0!B<jB:^B8RB33B2-B;dB<jB7LB2-B-B+B$�B+B,B/B.B5?B/B33B49B0!B+B�BVB�B!�B"�B"�B1'B0!B,B'�B�B�BPB\B1B�B�B�B{BoBoB�B"�B"�B!�B�BhB�B�BPBB�BuBuBVB�B �B�B�B�B$�B'�B&�B%�B&�B$�B!�B�BVB�B�B�B�B�B�BVBB+B�BoBPBoB&�B2-B5?B33B1'B,B"�B�B1'B8RB5?B5?B1'B(�B+B=qB:^B8RB1'B/B=qBB�BD�BF�B@�BD�B>wB@�B@�BM�BQ�BXBe`BbNB`BBgmBhsBffBbNBffBhsBe`BjBe`B]/Be`Bo�B{�B� B�B�B�B� B{�B{�B�+B�B�B{�Bz�Bx�B�VB��B��B��B��B��B��B��B��B��B��B��B��B��B�{B��B��B��B��B�!B�?B�9B�3B�3B�?B�XBBBŢBÖB��BB�wB��B�
B�B�B�)B�B�B�B�;B�sB�B�yB�B�yB��B��B��B��B��B	B	B	B	B	B	B	B	  B��B��B	B	B	B	+B	PB	hB	{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	#�B	)�B	0!B	1'B	49B	49B	8RB	9XB	;dB	?}B	@�B	C�B	D�B	D�B	D�B	D�B	F�B	I�B	L�B	O�B	O�B	O�B	P�B	Q�B	R�B	R�B	S�B	_;B	`BB	]/B	^5B	jB	p�B	x�B	z�B	z�B	z�B	z�B	{�B	{�B	{�B	{�B	z�B	}�B	�B	�B	�B	�B	�B	�+B	�%B	�=B	�=B	�%B	�B	�VB	�uB	��B	��B	��B	��B	��B	��B	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�'B	�-B	�3B	�3B	�-B	�3B	�3B	�3B	�'B	�?B	�LB	�LB	�RB	��B	ÖB	ÖB	B	��B	��B	ƨB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�B	�B	�B	�)B	�/B	�/B	�5B	�5B	�5B	�/B	�/B	�5B	�TB	�`B	�`B	�`B	�ZB	�ZB	�ZB	�TB	�TB	�ZB	�ZB	�HB	�ZB	�yB	�yB	�yB	�fB	�mB	�sB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
%B
%B
%B
%B
B
+B
1B
+B
	7B
	7B
DB
JB
JB
PB
PB
PB
\B
bB
VB
VB
hB
hB
hB
bB
bB
oB
oB
hB
\B
oB
{B
�B
{B
oB
hB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
!�B
�B
�B
!�B
!�B
#�B
$�B
$�B
$�B
%�B
$�B
#�B
$�B
$�B
"�B
#�B
#�B
'�B
(�B
)�B
)�B
(�B
'�B
&�B
(�B
(�B
+B
-B
.B
-B
-B
+B
+B
-B
.B
.B
-B
1'B
/B
0!B
0!B
0!B
/B
/B
0!B
/B
1'B
1'B
1'B
.B
+B
.B
33B
49B
5?B
6FB
6FB
6FB
5?B
5?B
49B
6FB
5?B
5?B
5?B
5?B
5?B
5?B
2-B
33B
49B
7LB
6FB
7LB
8RB
7LB
6FB
7LB
8RB
9XB
9XB
8RB
8RB
:^B
:^B
9XB
8RB
:^B
:^B
9XB
=qB
=qB
=qB
<jB
<jB
<jB
;dB
:^B
;dB
<jB
<jB
:^B
;dB
=qB
?}B
A�B
@�B
A�B
B�B
B�B
D�B
D�B
C�B
B�B
@�B
B�B
C�B
D�B
F�B
F�B
F�B
F�B
D�B
F�B
G�B
I�B
I�B
I�B
H�B
H�B
H�B
G�B
D�B
E�B
I�B
J�B
I�B
I�B
H�B
K�B
L�B
L�B
L�B
L�B
L�B
M�B
M�B
L�B
L�B
L�B
M�B
O�B
O�B
O�B
O�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
R�B
Q�B
Q�B
P�B
O�B
P�B
Q�B
R�B
R�B
R�B
Q�B
S�B
S�B
S�B
S�B
S�B
T�B
S�B
S�B
VB
VB
T�B
W
B
W
B
VB
T�B
W
B
XB
XB
YB
ZB
ZB
ZB
ZB
YB
XB
XB
\)B
]/B
]/B
]/B
\)B
]/B
\)B
\)B
[#B
ZB
ZB
ZB
]/B
\)B
[#B
ZB
]/B
_;B
_;B
_;B
_;B
`BB
aHB
aHB
aHB
aHB
aHB
aHB
`BB
`BB
_;B
^5B
_;B
`BB
`BB
aHB
`BB
_;B
^5B
bNB
dZB
dZB
e`B
e`B
e`B
dZB
dZB
cTB
cTB
ffB
e`B
cTB
dZB
gmB
hsB
hsB
hsB
hsB
gmB
gmB
gmB
ffB
gmB
gmB
gmB
iyB
jB
jB
iyB
hsB
iyB
jB
k�B
l�B
l�B
k�B
k�B
k�B
l�B
l�B
k�B
l�B
m�B
m�B
l�B
l�B
m�B
p�B
p�B
p�B
p�B
o�B
n�B
n�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
q�B
p�B
p�B
p�B
o�B
p�B
p�B
o�B
n�B
n�B
q�B
r�1111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�_B�yB�sB�mB�mB�mB�mB�B�mB�mB�RB�fB�ZB�bBۦB�IB��G�O�G�O�B{�B�RB�B�B��B��B�SB�5B�5B��B�(B�=B|�BnIB}"Bj�BK^BPbBS�BFBB�B7�B+kB*BMB�B4B�B�B��B�5B��B��B��B�~B�4B��B��B��Bl�Bf2BRBV9BCGB'�B�B%B
��B
��B
�rB
�NB
�VB
�tB
�aB
��B
�;B
b�B
r�B
S[B
C{B
>�B
>�B
2-B
	B
�B

=B	�6B	�B
�B	�B	��B	�B	߾B	�6B	�0B	�FB	�B	�MB	��B	ϑB	ŢB	��B	��B	�sB	��B	��B	��B	��B	��B	��B	��B	��B	�^B	x�B	m�B	sB	y�B	zxB	w�B	t9B	eFB	I�B	"�B	�B	fB	*�B	=�B	3B	.B	2|B	/�B	%�B	�B�(B��B	�B	'B�AB�#B�OB�IB�B�B�;B�B�
B�XB�oBуBɠB�B�4B�WB��B��B��B�[B�B��B��B��B�Bo�B�0B�B��B}"Bh�B[�Bi�Br|Bo�Bg�B`�Bn/BwBv�Bq�BgBQ�B:�BY�BJ�BA;BDB=<BGBN"BW�Bb�Be�Bf�Be�Bd�B`�B]�BX�BO�BO�BA B1�BL�BZ7BZkBUMBMPB;B5�B<jB/�B.�B%�B2-B5?B2aB9	B88B.cB7B1�B<�B;B9XB49B3MB;�B<�B7�B2�B-�B,B&B,B,�B/�B/ B5�B0!B3�B4�B0�B+�B�B.B�B"�B#�B#�B1�B0�B,qB(XB �B�B�B�B	�BdB~BxB�B�BuB5B#:B# B"4BeBoBB1BpB�B�BFBFB\B5B!B/B=B�B%B(>B'B&2B'8B%,B"B=B�B	BmBYB5B=B	B\B�BfB$BuB�B�B'�B2|B5tB3�B1vB,�B#�B �B1�B8�B5�B5�B1�B*0B,"B=�B:�B8�B2-B0;B>BCBEBF�BA;BE9B?}BAoBA�BNpBR�BX�Be`Bb�B`�Bg�Bh�Bf�Bb�Bf�Bh�Be�Bj�Be�B^jBfLBpUB|PB�4B�MB�3B�AB�4B|jB|�B�+B�SB�oB|�B{�BzB��B��B��B��B��B��B��B��B��B��B��B��B��B�
B�2B�mB�IB�RB�eB��B�ZB��B��B��B��B��BªB��BżB��B� B�B�HB�JB�$B�KB�kB�CB�kBؓBخB߾B�B�B��B��B�B��B�B�B�B�(B	B	;B	 B	 B	'B	'B	 B	 B�.B�HB	UB	UB	mB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	B	/B	$@B	*0B	0;B	1[B	4TB	4�B	8�B	9�B	;�B	?�B	@�B	C�B	D�B	D�B	D�B	D�B	F�B	I�B	MB	PB	O�B	PB	QB	R B	S&B	SuB	T{B	_VB	`vB	]�B	^�B	j�B	p�B	x�B	z�B	z�B	z�B	z�B	|B	|B	|B	|B	{0B	~(B	�3B	�MB	�MB	�GB	�MB	�_B	�YB	�=B	�rB	�tB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�B	�B	�"B	�B	�=B	�CB	�)B	�=B	�6B	�5B	�;B	�[B	�-B	�3B	�MB	�|B	�MB	�MB	�MB	�[B	�ZB	��B	��B	��B	��B	ÖB	ÖB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�"B	�0B	�(B	� B	�@B	�B	�+B	�$B	�1B	�1B	�KB	�eB	�CB	�IB	�IB	�jB	�jB	�OB	�dB	�~B	ބB	�nB	�`B	�zB	�`B	�tB	�tB	�tB	�nB	�B	�tB	�tB	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	�B	��B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�%B	�	B	��B	�B	�B	�B	�B	�B	�B	��B	�(B	�B	�"B	�<B	�JB	�.B
 4B
;B
'B
-B
-B
-B
AB
[B
?B
YB
?B
YB
mB
_B
KB
_B
	RB
	lB
^B
~B
~B
jB
�B
�B
vB
�B
�B
�B
�B
�B
�B
}B
�B
oB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
!�B
 B
�B
!�B
"B
#�B
%B
%B
$�B
%�B
$�B
$B
%B
%B
# B
$B
$B
(
B
)*B
)�B
*B
)B
(>B
'B
)*B
)*B
+B
-)B
./B
-CB
-CB
+QB
+6B
-)B
.IB
./B
-CB
1AB
/5B
0;B
0;B
0;B
/5B
/iB
0UB
/OB
1AB
1AB
1AB
.IB
+QB
.cB
3MB
4TB
5ZB
6FB
6FB
6`B
5ZB
5ZB
4TB
6FB
5ZB
5ZB
5tB
5tB
5tB
5?B
2|B
3hB
4nB
7LB
6zB
7�B
8lB
7fB
6zB
7fB
8�B
9XB
9�B
8lB
8lB
:^B
:^B
9�B
8�B
:�B
:�B
9�B
=qB
=�B
=qB
<�B
<�B
<�B
;�B
:�B
;B
<�B
<�B
:�B
;�B
=�B
?�B
A�B
@�B
A�B
B�B
B�B
D�B
D�B
C�B
B�B
@�B
B�B
C�B
D�B
F�B
F�B
F�B
F�B
D�B
F�B
G�B
I�B
I�B
I�B
H�B
H�B
H�B
G�B
D�B
E�B
I�B
J�B
I�B
I�B
H�B
K�B
L�B
L�B
L�B
L�B
MB
M�B
M�B
L�B
L�B
L�B
M�B
O�B
O�B
O�B
O�B
P�B
Q B
Q B
Q�B
RB
Q�B
SB
RB
RB
QB
O�B
QB
RB
R�B
S&B
SB
RB
S�B
T,B
TB
TB
T,B
UB
TB
T,B
VB
VB
U2B
W$B
W$B
V9B
U2B
W$B
X+B
X+B
Y1B
Z7B
Z7B
ZB
Z7B
YKB
X_B
XEB
\)B
]/B
]/B
]/B
\)B
]IB
\CB
\)B
[=B
ZQB
ZkB
ZQB
]/B
\]B
[=B
ZQB
]dB
_;B
_VB
_pB
_pB
`vB
aHB
abB
aHB
aHB
abB
aHB
`BB
`\B
_pB
^jB
_VB
`vB
`vB
abB
`vB
_pB
^�B
bhB
dZB
dtB
ezB
e`B
e`B
dtB
dtB
c�B
c�B
ffB
ezB
c�B
d�B
g�B
h�B
hsB
hsB
hsB
g�B
g�B
g�B
f�B
g�B
g�B
g�B
iyB
jB
jB
i�B
h�B
i�B
j�B
k�B
l�B
l�B
k�B
k�B
k�B
l�B
l�B
k�B
l�B
m�B
m�B
l�B
l�B
m�B
p�B
p�B
p�B
p�B
o�B
n�B
n�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
q�B
p�B
p�B
p�B
o�B
p�B
p�B
o�B
n�B
n�B
q�B
r�1111111111111111144111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.07(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201805030036032018050300360320180503003603201806221241092018062212410920180622124109201806042119312018060421193120180604211931  JA  ARFMdecpA19c                                                                20180429063517  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180428213519  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180428213522  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180428213523  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180428213523  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180428213523  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180428213524  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcp2.8e                                                                20180428213524  QCF$                G�O�G�O�G�O�            4000JA  ARGQaqcp2.8e                                                                20180428213524  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180428213524  QCF$                G�O�G�O�G�O�            4000JA  ARGQrqcpt16c                                                                20180428213525  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180428213525                      G�O�G�O�G�O�                JA  ARUP                                                                        20180428215634                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180429153201  CV  JULD            G�O�G�O�F���                JM  ARSQJMQC2.0                                                                 20180430000000  CF  PSAL_ADJUSTED_QCB   B   G�O�                JM  ARSQJMQC2.0                                                                 20180430000000  CF  TEMP_ADJUSTED_QCB   B   G�O�                JM  ARCAJMQC2.0                                                                 20180502153603  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180502153603  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180604121931  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622034109  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115131517                      G�O�G�O�G�O�                