CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       ~2019-02-12T15:37:08Z creation;2019-02-12T15:37:12Z conversion to V3.1;2019-12-18T07:17:00Z update;2022-11-21T05:29:24Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         BPRIMARY|https://orcid.org/0000-0001-9150-6442|Kanako Sato, JAMSTEC        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7H   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7X   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7\   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    7`   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7p   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  8   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8H   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    8L   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    8P   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     8T   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8t   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8x   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8|   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8�   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8�   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    9   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    9   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    9   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        :   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  :    PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]H   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  a0   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �X   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �@   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �h   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �P   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ΐ   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �0   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �@   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �D   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �H   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �L   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �P   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20190212153708  20221123111508  4902148 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I1_0397_164                     2C  DdBNAVIS_A                         0397                            ARGO 011514                     863 @ا��c 1   @ا u� @<^z�G��dB����1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @���@�33@���A   A@  A`  A�  A�  A�  A�33A�33A�  A�  A�  B ffB��B  B  B   B(  B0  B8  B@  BG��BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D fD � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ D�|�D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D��3D�  D�@ DɃ3D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��\@���@��\A�HA>�HA^�HA~�HA�p�A�p�A���A���A�p�A�p�A�p�B �BQ�B�RB�RB�RB'�RB/�RB7�RB?�RBGQ�BO�RBW�RB_�RBg�RBo�RBw�RB�RB��)B��)B��)B��)B��)B��)B��)B��)B��)B���B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B�\B��)B��)B��)B��)B��)B��)C�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
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
��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D �D {�D ��D!{�D!��D"{�D"��D#{�D#��D${�D$��D%{�D%��D&{�D&��D'{�D'��D({�D(��D){�D)��D*{�D*��D+{�D+��D,{�D,��D-{�D-��D.{�D.��D/{�D/��D0{�D0��D1{�D1��D2{�D2��D3{�D3��D4{�D4��D5{�D5��D6{�D6��D7{�D7��D8{�D8��D9{�D9��D:{�D:��D;{�D;��D<{�D<��D={�D=��D>{�D>��D?{�D?��D@{�D@��DA{�DA��DB{�DB��DC{�DC��DD{�DD��DE{�DE��DF{�DF��DG{�DG��DH{�DH��DI{�DI��DJ{�DJ��DK{�DK��DL{�DL��DM{�DM��DN{�DN��DO{�DO��DP{�DP��DQ{�DQ��DR{�DR��DS{�DS��DT{�DT��DU{�DU��DV{�DV��DW{�DW��DX{�DX��DY{�DY��DZ{�DZ��D[{�D[��D\{�D\��D]{�D]��D^{�D^��D_{�D_��D`{�D`��Da{�Da��Db{�Db��Dc{�Dc��Dd{�Dd��De{�De��Df{�Df��Dg{�Dg��Dh{�Dh��Di{�Di��Dj{�Dj��Dk{�Dk��Dl{�Dl��Dm{�Dm��Dn{�Dn��Do{�Do��Dp{�Dp��Dq{�Dq��Dr{�Dr��Ds{�Ds��Dt{�Dt��Du{�Du��Dv{�Dv��Dw{�Dw��Dx{�Dx��Dy{�Dy��Dz{�Dz��D{{�D{��D|{�D|��D}{�D}��D~{�D~��D{�D��D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D½�D���D�=�D�}�Dý�D���D�=�D�}�DĽ�D���D�=�D�z�DŽ�D���D�=�D�}�Dƽ�D���D�=�D�}�Dǽ�D���D�=�D�}�D���D���D�=�Dɀ�Dɽ�D���D�=�D�}�Dʽ�D���D�=�D�}�D˽�D���D�=�D�}�D̽�D���D�=�D�}�Dͽ�D���D�=�D�}�Dν�D���D�=�D�}�DϽ�D���D�=�D�}�Dн�D���D�=�D�}�Dѽ�D���D�=�D�}�Dҽ�D���D�=�D�}�Dӽ�D���D�=�D�}�DԽ�D���D�=�D�}�Dս�D���D�=�D�}�Dֽ�D���D�=�D�}�D׽�D���D�=�D�}�Dؽ�D���D�=�D�}�Dٽ�D���D�=�D�}�Dڽ�D���D�=�D�}�D۽�D���D�=�D�}�Dܽ�D���D�=�D�}�Dݽ�D���D�=�D�}�D޽�D���D�=�D�}�D߽�D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D���D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�@�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��A��\A��hA��\A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��A��!A��!A��-A��-A��!A���A�l�A���A�p�A��A�|�A�G�A��A��DA�S�A�E�A�9XA�1'A�&�A�%A���A���A�A���A�-A���A�XA�p�A��RA�l�A���A�^5A��hA�33A���A���A�5?A��7A�oA���A�7LA�A�A�bA�7LA�v�A�\)A�?}A�K�A�Q�A�%A�jA��A�p�A�1A|�A+A~ffA|bNAz�`Az(�AyhsAw�hAv5?Au;dAsG�Ar��Aql�Ap^5Ao��Ao+An-Al�Aj��Ai�-Ai?}Ai
=Ah��Ah��Ah�Ag&�AfI�Ae�PAd1Ab�/AbAat�Aa�A`��A_�A^~�A]�TA]O�A\��A[��AZ��AZr�AZM�AY�AX(�AW|�AW`BAWC�AW
=AV��AV^5AU�AUƨAU7LASƨAQAP�AP�uAP5?AO�-AOG�AN5?AL�AL5?AK��AH��AG&�AES�ADbNAC�mAC\)AB�/ABn�AB1AA�A@Q�A>^5A;G�A9�mA9p�A8�9A7A6�jA6(�A5S�A4��A3�wA3/A3%A2��A2�+A1��A/�PA.~�A-��A-l�A,��A, �A+t�A*ĜA*A)��A)�A(r�A'�^A&��A%��A$ �A"ȴA"{A!A (�A JA�FA�9A(�A��A"�A�RAVAdZA��A �A��A��A/A��A��A�A�jA�An�AM�A-A�mA�PAC�A��AC�A��A=qA+A�A9XA�AK�A
��A
ZA	��A	�7A	XA�HA1'A�;A��A%AM�A�AA��A�AbNA�Ar�AhsA �A v�A @�7L@��@�J@��@�A�@�|�@�n�@��@�+@�%@��@�o@�M�@���@�@�+@�7L@�  @�+@�^@�?}@��@䛦@�z�@�I�@�\@���@�;d@ް!@�hs@�Z@۝�@���@�J@���@��@�@ӶF@�n�@��@ϥ�@Ώ\@���@Ɂ@�/@ț�@�(�@��
@�
=@�Ĝ@�$�@��@���@��@�o@�@��@�X@��@� �@��m@�t�@�$�@���@�$�@��@�Ĝ@�1@�ƨ@��@�K�@�"�@��y@��!@�n�@�=q@�@��h@�hs@�?}@�V@��9@�Q�@��w@�"�@�=q@�Q�@�E�@��j@���@���@�t�@�C�@�@���@�33@��H@���@�v�@�n�@�n�@�^5@�5?@�@�hs@�hs@�O�@�G�@�?}@�/@�V@���@��/@��j@��u@�bN@���@�t�@��@�$�@� �@�dZ@�ff@�{@���@�p�@�hs@�`B@�X@�X@�?}@��j@�I�@� �@���@��w@���@�t�@�\)@�o@�V@���@��^@�X@��@�bN@�1@��w@��@���@�|�@�C�@�o@���@�V@��@�p�@�?}@�7L@�&�@���@�Ĝ@�z�@��;@��@�\)@�+@���@�=q@�O�@��9@�j@�(�@�(�@�(�@�1'@�1'@�A�@�I�@��@���@�|�@�dZ@�\)@�S�@�K�@�C�@�;d@�+@�@��y@��!@�E�@��@��@���@��-@���@���@���@�?}@���@��u@�bN@�  @K�@~�R@~V@~5?@}�T@}�-@}/@|��@|9X@{��@z��@y��@x��@vV@up�@t�@tz�@tZ@tI�@tI�@t1@s�m@s�m@sƨ@s��@s�@sS�@sS�@sS�@sS�@s@r��@r��@r~�@r=q@qG�@pb@o�@o\)@n�y@n��@nff@m@mp�@m�@lz�@l��@l�@l�/@l(�@k�m@k�F@kt�@k33@j�@j�\@j=q@iX@hr�@hA�@h1'@g��@g�P@gl�@g+@f��@f��@f�y@f�@fȴ@f��@f��@fv�@f5?@e@e?}@d��@d��@c�
@cdZ@cC�@cC�@c33@cC�@b�H@b~�@b-@a��@a��@a7L@a%@`Ĝ@`r�@`b@_|�@_�@^V@]�T@]O�@]�@\��@\�j@\9X@\1@[�F@[t�@[S�@[33@Z�\@Z=q@Y�@Y7L@X��@X�u@Xr�@XA�@X �@X �@X �@X �@W��@W�@V�R@V��@V�+@Vv�@V5?@U��@U��@Up�@UO�@U/@T�/@T�D@TZ@TI�@T9X@T(�@S��@Sƨ@S�@R�H@Q��@Q�7@Qx�@Q�@P�9@P�9@P�u@P1'@P  @O�w@O+@O�@N�@N��@N@M/@Lz�@L(�@K��@K�@Ko@J�@Jn�@I��@I��@I�7@IG�@I&�@I%@H��@H�`@H�`@HĜ@H�u@H�@HQ�@H �@Hb@G�@G��@G�w@Gl�@G+@F�@F�R@Fff@E�@E�T@E��@E��@EO�@E�@D�@D�/@D�j@D�D@DZ@D1@Cƨ@CdZ@C@B�H@B~�@B�@A��@A��@A&�@@��@@��@@�@?�w@?|�@?\)@?+@?�@>��@>E�@>@>@>@=��@=�-@=��@=?}@=V@<�/@<�D@<1@;��@;�F@;�@;dZ@;"�@:��@:��@:�!@:��@:��@:~�@:-@9�7@8�`@8�9@8bN@8Q�@8Q�@8A�@8 �@8 �@8b@8  @7�w@7��@7l�@7\)@7;d@6ȴ@5�@5O�@4j@3�F@3��@3t�@3o@2�\@2^5@1��@1��@1X@1�@0Ĝ@0��@0�u@0r�@01'@/�@/�;@/�@/|�@.��@.v�@.5?@-�@-�-@-`B@,�@+��@+"�@*��@*��@*~�@*n�@*-@*�@*J@)�^@)�7@)G�@)7L@)7L@)&�@)&�@)&�@(�`@(�u@'�;@';d@&��@&�@&��@&v�@&V@&E�@&5?@&$�@&$�@%�@%��@%�h@%�h@%�h@%�@%`B@%`B@%`B@%O�@%/@$��@$Z@#��@#33@"�@"�H@"�H@"��@"��@"�!@"��@"^5@"^5@"=q@!�@!X@!%@!%@ �`@ �u@ r�@ bN@ 1'@   @�w@\)@+@�@
=@��@�@�R@��@�+@ff@E�@$�@�@��@�-@�@p�@O�@��@�@�/@Z@�m@ƨ@ƨ@��@�@dZ@��@^5@hs@�`@�9@�@Q�@ �@�;@��@�w@�@��@�P@;d@�@�@��@E�@{@�@V@�/@��@�D@9X@��@�F@dZ@C�@"�@o@�H@��@~�@^5@M�@=q@-@-@��@��@�^@��@��@hs@7L@Ĝ@�9@b@�@��@�P@|�@K�@;d@�@��@V@V@$�@�@@��@�@p�@p�@`B@?}@�@�@��@�@j@9X@(�@�m@�F@��@t�@C�@"�@
�@
�H@
��@
��@
��@
��@
n�@
�@	�#@	�#@	��@	��@	�^@	x�@	7L@	&�@	�@	�@	�@	�@	�@	%@��@�`@�`@��@Ĝ@�u@�@Q�@ �@  @�@�w@��@\)@��@�@ȴ@��@��@v�@5?@$�@{@{@{@@�T@@��@�@`B@�@�@�j@��@��@z�@�@�F@�@dZ@t�@t�@t�@dZ@dZ@S�@S�@S�@C�@C�@C�@"�@�@�H@��@��@�\@�\1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��A��\A��hA��\A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��A��!A��!A��-A��-A��!A���A�l�A���A�p�A��A�|�A�G�A��A��DA�S�A�E�A�9XA�1'A�&�A�%A���A���A�A���A�-A���A�XA�p�A��RA�l�A���A�^5A��hA�33A���A���A�5?A��7A�oA���A�7LA�A�A�bA�7LA�v�A�\)A�?}A�K�A�Q�A�%A�jA��A�p�A�1A|�A+A~ffA|bNAz�`Az(�AyhsAw�hAv5?Au;dAsG�Ar��Aql�Ap^5Ao��Ao+An-Al�Aj��Ai�-Ai?}Ai
=Ah��Ah��Ah�Ag&�AfI�Ae�PAd1Ab�/AbAat�Aa�A`��A_�A^~�A]�TA]O�A\��A[��AZ��AZr�AZM�AY�AX(�AW|�AW`BAWC�AW
=AV��AV^5AU�AUƨAU7LASƨAQAP�AP�uAP5?AO�-AOG�AN5?AL�AL5?AK��AH��AG&�AES�ADbNAC�mAC\)AB�/ABn�AB1AA�A@Q�A>^5A;G�A9�mA9p�A8�9A7A6�jA6(�A5S�A4��A3�wA3/A3%A2��A2�+A1��A/�PA.~�A-��A-l�A,��A, �A+t�A*ĜA*A)��A)�A(r�A'�^A&��A%��A$ �A"ȴA"{A!A (�A JA�FA�9A(�A��A"�A�RAVAdZA��A �A��A��A/A��A��A�A�jA�An�AM�A-A�mA�PAC�A��AC�A��A=qA+A�A9XA�AK�A
��A
ZA	��A	�7A	XA�HA1'A�;A��A%AM�A�AA��A�AbNA�Ar�AhsA �A v�A @�7L@��@�J@��@�A�@�|�@�n�@��@�+@�%@��@�o@�M�@���@�@�+@�7L@�  @�+@�^@�?}@��@䛦@�z�@�I�@�\@���@�;d@ް!@�hs@�Z@۝�@���@�J@���@��@�@ӶF@�n�@��@ϥ�@Ώ\@���@Ɂ@�/@ț�@�(�@��
@�
=@�Ĝ@�$�@��@���@��@�o@�@��@�X@��@� �@��m@�t�@�$�@���@�$�@��@�Ĝ@�1@�ƨ@��@�K�@�"�@��y@��!@�n�@�=q@�@��h@�hs@�?}@�V@��9@�Q�@��w@�"�@�=q@�Q�@�E�@��j@���@���@�t�@�C�@�@���@�33@��H@���@�v�@�n�@�n�@�^5@�5?@�@�hs@�hs@�O�@�G�@�?}@�/@�V@���@��/@��j@��u@�bN@���@�t�@��@�$�@� �@�dZ@�ff@�{@���@�p�@�hs@�`B@�X@�X@�?}@��j@�I�@� �@���@��w@���@�t�@�\)@�o@�V@���@��^@�X@��@�bN@�1@��w@��@���@�|�@�C�@�o@���@�V@��@�p�@�?}@�7L@�&�@���@�Ĝ@�z�@��;@��@�\)@�+@���@�=q@�O�@��9@�j@�(�@�(�@�(�@�1'@�1'@�A�@�I�@��@���@�|�@�dZ@�\)@�S�@�K�@�C�@�;d@�+@�@��y@��!@�E�@��@��@���@��-@���@���@���@�?}@���@��u@�bN@�  @K�@~�R@~V@~5?@}�T@}�-@}/@|��@|9X@{��@z��@y��@x��@vV@up�@t�@tz�@tZ@tI�@tI�@t1@s�m@s�m@sƨ@s��@s�@sS�@sS�@sS�@sS�@s@r��@r��@r~�@r=q@qG�@pb@o�@o\)@n�y@n��@nff@m@mp�@m�@lz�@l��@l�@l�/@l(�@k�m@k�F@kt�@k33@j�@j�\@j=q@iX@hr�@hA�@h1'@g��@g�P@gl�@g+@f��@f��@f�y@f�@fȴ@f��@f��@fv�@f5?@e@e?}@d��@d��@c�
@cdZ@cC�@cC�@c33@cC�@b�H@b~�@b-@a��@a��@a7L@a%@`Ĝ@`r�@`b@_|�@_�@^V@]�T@]O�@]�@\��@\�j@\9X@\1@[�F@[t�@[S�@[33@Z�\@Z=q@Y�@Y7L@X��@X�u@Xr�@XA�@X �@X �@X �@X �@W��@W�@V�R@V��@V�+@Vv�@V5?@U��@U��@Up�@UO�@U/@T�/@T�D@TZ@TI�@T9X@T(�@S��@Sƨ@S�@R�H@Q��@Q�7@Qx�@Q�@P�9@P�9@P�u@P1'@P  @O�w@O+@O�@N�@N��@N@M/@Lz�@L(�@K��@K�@Ko@J�@Jn�@I��@I��@I�7@IG�@I&�@I%@H��@H�`@H�`@HĜ@H�u@H�@HQ�@H �@Hb@G�@G��@G�w@Gl�@G+@F�@F�R@Fff@E�@E�T@E��@E��@EO�@E�@D�@D�/@D�j@D�D@DZ@D1@Cƨ@CdZ@C@B�H@B~�@B�@A��@A��@A&�@@��@@��@@�@?�w@?|�@?\)@?+@?�@>��@>E�@>@>@>@=��@=�-@=��@=?}@=V@<�/@<�D@<1@;��@;�F@;�@;dZ@;"�@:��@:��@:�!@:��@:��@:~�@:-@9�7@8�`@8�9@8bN@8Q�@8Q�@8A�@8 �@8 �@8b@8  @7�w@7��@7l�@7\)@7;d@6ȴ@5�@5O�@4j@3�F@3��@3t�@3o@2�\@2^5@1��@1��@1X@1�@0Ĝ@0��@0�u@0r�@01'@/�@/�;@/�@/|�@.��@.v�@.5?@-�@-�-@-`B@,�@+��@+"�@*��@*��@*~�@*n�@*-@*�@*J@)�^@)�7@)G�@)7L@)7L@)&�@)&�@)&�@(�`@(�u@'�;@';d@&��@&�@&��@&v�@&V@&E�@&5?@&$�@&$�@%�@%��@%�h@%�h@%�h@%�@%`B@%`B@%`B@%O�@%/@$��@$Z@#��@#33@"�@"�H@"�H@"��@"��@"�!@"��@"^5@"^5@"=q@!�@!X@!%@!%@ �`@ �u@ r�@ bN@ 1'@   @�w@\)@+@�@
=@��@�@�R@��@�+@ff@E�@$�@�@��@�-@�@p�@O�@��@�@�/@Z@�m@ƨ@ƨ@��@�@dZ@��@^5@hs@�`@�9@�@Q�@ �@�;@��@�w@�@��@�P@;d@�@�@��@E�@{@�@V@�/@��@�D@9X@��@�F@dZ@C�@"�@o@�H@��@~�@^5@M�@=q@-@-@��@��@�^@��@��@hs@7L@Ĝ@�9@b@�@��@�P@|�@K�@;d@�@��@V@V@$�@�@@��@�@p�@p�@`B@?}@�@�@��@�@j@9X@(�@�m@�F@��@t�@C�@"�@
�@
�H@
��@
��@
��@
��@
n�@
�@	�#@	�#@	��@	��@	�^@	x�@	7L@	&�@	�@	�@	�@	�@	�@	%@��@�`@�`@��@Ĝ@�u@�@Q�@ �@  @�@�w@��@\)@��@�@ȴ@��@��@v�@5?@$�@{@{@{@@�T@@��@�@`B@�@�@�j@��@��@z�@�@�F@�@dZ@t�@t�@t�@dZ@dZ@S�@S�@S�@C�@C�@C�@"�@�@�H@��@��@�\@�\1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111BBBBB��BBBBBBBBBBBBBBBBBBBBBB��BBBBBBB��BÖB�jB�wB�9B�3B�FB��B��B��B��B��B��B��B��B�hB�BjBVB)�BVB�yB�#B��B��B��B�oB�Bx�Bp�BbNB2-B �B�BhB\BB
��B
�TB
�/B
��B
��B
�qB
��B
��B
��B
��B
�uB
�DB
�%B
�B
�B
u�B
jB
_;B
[#B
S�B
E�B
@�B
<jB
2-B
,B
%�B
�B
�B
�B
uB
%B	��B	��B	��B	��B	��B	��B	��B	�yB	�NB	�B	��B	ƨB	��B	�jB	�qB	�LB	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�VB	�JB	�JB	�DB	�=B	�7B	�B	�B	�B	|�B	s�B	hsB	dZB	aHB	]/B	W
B	YB	J�B	D�B	@�B	2-B	&�B	 �B	�B	bB	JB	%B	B��B��B��B�B�ZB�)B�)B�5B�/B�B�
B��B��B��BĜBĜBĜBǮB��BȴB�}B�qB�dB�XB�FB�3B�3B�B�B�B��B��B��B��B��B�hB�DB�JB�1B�7B�JB�DB�1B�1B�+B�B�%B�B�B|�B{�B}�Bv�Br�Br�Bm�Bl�BjBiyBiyBhsBiyBgmBgmBiyBe`BbNB`BBe`B^5B[#BS�BR�BS�BQ�BO�BM�BM�BN�BL�BI�BI�BK�BH�BE�BD�BF�BE�BC�BB�BI�B?}B@�B=qBA�B>wB<jB>wB8RB9XB7LB7LB8RB8RB7LB6FB6FB5?B49B8RB6FB6FB2-B.B-B+B+B)�B)�B)�B-B&�B!�B"�B"�B"�B!�B!�B!�B"�B �B#�B�B �B�B �B�B)�B!�B"�B#�B#�B$�B(�B0!B0!B.B/B0!B/B7LB7LB;dB7LB7LB7LB8RB:^B:^BM�BN�BO�BQ�BR�BR�BS�BS�BS�BT�BT�BVBVBW
BW
BW
BXBYBYB\)B\)B`BBffBn�Bp�Bp�Bp�Bp�Bp�Bz�B�7B�JB�PB�VB�\B�bB�bB�hB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�XB�XB�wB�qB�wB�wB�}B�}B�}B�}B�}BŢBƨBǮBǮBǮBǮBǮBǮBǮB��B��B��B��B��B�
B�B�B�B�#B�)B�5B�;B�HB�ZB�mB�B�B�B�B�B�B�B��B��B��B��B	B	%B	VB	bB	oB	�B	�B	�B	�B	�B	�B	�B	�B	"�B	"�B	#�B	$�B	%�B	%�B	'�B	'�B	(�B	)�B	)�B	-B	0!B	1'B	33B	49B	49B	49B	49B	5?B	8RB	;dB	=qB	>wB	A�B	D�B	F�B	G�B	H�B	I�B	J�B	L�B	N�B	P�B	R�B	W
B	ZB	_;B	iyB	jB	l�B	n�B	n�B	n�B	n�B	o�B	p�B	p�B	p�B	q�B	q�B	r�B	r�B	r�B	r�B	t�B	t�B	t�B	u�B	v�B	z�B	|�B	~�B	� B	�B	�B	�B	�%B	�+B	�1B	�7B	�+B	�1B	�1B	�DB	�DB	�DB	�JB	�PB	�VB	�\B	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�!B	�'B	�-B	�-B	�9B	�?B	�FB	�LB	�RB	�^B	�dB	�qB	�}B	��B	ÖB	ÖB	ĜB	ŢB	ƨB	ǮB	ȴB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�#B	�#B	�)B	�/B	�/B	�/B	�;B	�;B	�BB	�BB	�BB	�BB	�HB	�NB	�NB	�`B	�sB	�sB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
+B
+B
+B
1B
	7B
	7B
	7B

=B

=B

=B
JB
JB
PB
PB
VB
\B
\B
bB
bB
hB
oB
oB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
"�B
"�B
#�B
#�B
"�B
#�B
#�B
#�B
#�B
#�B
#�B
$�B
$�B
#�B
#�B
$�B
%�B
'�B
)�B
+B
+B
+B
,B
-B
-B
-B
.B
/B
0!B
1'B
1'B
1'B
2-B
2-B
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
8RB
:^B
;dB
;dB
;dB
<jB
<jB
<jB
<jB
<jB
=qB
=qB
>wB
>wB
>wB
>wB
>wB
>wB
>wB
>wB
@�B
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
C�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
E�B
F�B
F�B
G�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
I�B
H�B
I�B
I�B
J�B
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
M�B
N�B
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
P�B
P�B
P�B
P�B
P�B
P�B
Q�B
R�B
R�B
R�B
R�B
R�B
R�B
S�B
T�B
VB
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
YB
YB
YB
ZB
ZB
ZB
[#B
[#B
[#B
\)B
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
bNB
bNB
bNB
cTB
dZB
dZB
dZB
dZB
dZB
dZB
e`B
e`B
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
gmB
gmB
gmB
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
n�B
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
p�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
t�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
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
v�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�uBBB�uB��BBBBªBBªBBªBªBBªBBBBBBBBBBB��BBBªBªBªB��B�AB��B��B��B��B�B�lB�B�-B��B��B��B��B�7B��B�@B��Bn}BZ�B-CB�B�WB޸BāB�QB�TB�,B��B{dBu%Bf�B3�B"B�B�B�B�B
��B
�B
��B
�pB
��B
�HB
�6B
�kB
��B
��B
�aB
��B
��B
�mB
�MB
w�B
k�B
`�B
]/B
U�B
G+B
B�B
=qB
3�B
-CB
&�B
�B
)B
�B
MB
_B	�B	�$B	�B	�8B	��B	�FB	��B	�B	�B	�qB	�B	�zB	� B	�VB	�B	�B	��B	��B	��B	��B	��B	��B	�#B	��B	�vB	�?B	��B	��B	��B	��B	��B	��B	��B	�MB	�B	.B	t�B	iB	d�B	a�B	^B	X�B	ZkB	K�B	F%B	C�B	4nB	(�B	!�B	9B	B	B	�B	�B	 B��B��B�B�B�B�IBߊB�jB��B�B��B�B�jB�B�B�mB�RB�B�	B�iB�BB�jB�DB�LB�9B�B��B��B�B�$B�sB�ZB��B�OB��B��B�PB��B��B��B��B��B�B��B��B�EB�'B��B}�B}�B�Bw�Bs�Bs�BnBl�Bj�Bi�Bi�Bh�BjBh$Bh�BkBfLBc�BbBg�B_pB\)BT�BS�BT�BR�BPHBNVBN�BO�BMjBJXBJ�BL�BIBFBESBGzBF�BD�BD�BJ�B@iBAUB>wBCGB?}B=�B?.B9$B:B8RB8�B9�B9rB8B7B6�B5�B5tB9�B7�B72B2�B/ B-wB+QB+kB*KB*�B+kB.�B'mB"�B#�B#�B#�B"�B"�B"�B$&B"B%FB �B!�B �B!�B �B+6B"4B#TB$@B$tB%�B*�B1�B0�B.�B0B0�B0�B8�B8lB;�B7�B7�B7�B9rB;�B<�BPBO�BPbBRTBS@BS@BT,BT,BTaBU2BUMBVSBVmBWYBW?BWYBX�BYBY�B\�B]Ba�Bg�Bo�Bq[Bp�Bp�Bq'Bq�B|6B��B��B��B��B��B�}B��B��B��B��B��B��B��B��B��B�B��B��B��B�B�:B�`B�fB�sB��B�oB��B�B��B��B��B��B��B��B��B��B� B��B��B��B��B��B��B��B��B�1B�B�6B�<B�hB�aB�sB�eB�7B�7B�qB�xBބBߊB�B��B�B�B�B�B��B��B��B�9B�*B�<B�]B�cB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	 'B	#B	"�B	$B	$�B	%�B	%�B	($B	($B	)*B	*B	*KB	-wB	0UB	1[B	3MB	4TB	4TB	4TB	4nB	5�B	8�B	;�B	=�B	>�B	A�B	D�B	F�B	G�B	H�B	I�B	J�B	MB	OBB	Q4B	S[B	WsB	Z�B	`'B	i�B	j�B	l�B	n�B	n�B	n�B	n�B	o�B	p�B	p�B	p�B	q�B	q�B	r�B	r�B	r�B	r�B	t�B	t�B	t�B	u�B	w2B	{dB	}<B	.B	�4B	�'B	�-B	�gB	�?B	�_B	�fB	�B	�EB	�KB	��B	�^B	�xB	�xB	�dB	�jB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�>B	�DB	�"B	�CB	�B	�IB	�/B	�cB	�UB	�AB	�GB	�aB	�TB	�ZB	�zB	��B	��B	��B	��B	��B	��B	��B	��B	ðB	ĶB	��B	��B	��B	��B	��B	��B	�	B	��B	�B	�(B	�B	� B	�&B	�B	�B	��B	�B	�,B	�MB	�SB	�+B	�1B	�1B	�1B	�7B	�WB	�=B	�CB	�IB	�IB	�IB	�VB	�VB	�BB	�\B	�vB	�\B	�bB	�B	�B	�B	�B	�B	��B	�B	�B	�B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	�B	�B	�	B	�B	�*B	�B	�"B	�(B	�(B	�B	�B	��B	�B	��B	�.B
 B
 B
 B
 B
 B
'B
'B
'B
AB
-B
3B
9B
mB
YB
+B
_B
EB
EB
KB
	lB
	lB
	RB

rB

rB

rB
dB
~B
jB
jB
�B
vB
vB
}B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
B
 B
!B
"�B
"�B
#�B
#�B
#B
#�B
#�B
#�B
#�B
$B
#�B
$�B
$�B
$B
$B
%FB
&LB
(>B
*0B
+6B
+B
+6B
,=B
-CB
-CB
-CB
.IB
/5B
0;B
1[B
1[B
1AB
2GB
2aB
3MB
3hB
4TB
4nB
4�B
5ZB
6zB
6`B
7�B
7�B
8�B
:�B
;B
;�B
;�B
<�B
<�B
<�B
<�B
<�B
=�B
=�B
>wB
>�B
>wB
>�B
>�B
>�B
>�B
>�B
@�B
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
C�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
E�B
F�B
F�B
G�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
I�B
H�B
I�B
J	B
J�B
K�B
K�B
K�B
L�B
L�B
L�B
MB
MB
L�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
OB
OB
N�B
O�B
O�B
O�B
PB
PB
QB
Q B
Q B
QB
Q B
Q4B
RB
SB
R�B
S&B
S&B
SB
S@B
TFB
UMB
VSB
W$B
X+B
X+B
X+B
X+B
YB
YB
YB
YB
YKB
Y1B
Y1B
ZQB
Z7B
Z7B
[=B
[=B
[qB
\CB
]IB
]IB
]IB
]dB
^OB
^jB
_VB
_VB
_VB
_VB
_VB
`vB
`vB
`BB
`\B
`BB
`\B
`vB
abB
aHB
aHB
aHB
abB
abB
b�B
b�B
b�B
cnB
dZB
dtB
dtB
d�B
d�B
dtB
ezB
ezB
ffB
f�B
f�B
f�B
f�B
g�B
g�B
gmB
gmB
g�B
g�B
gmB
g�B
g�B
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
jB
j�B
jB
jB
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
m�B
m�B
m�B
m�B
n�B
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
p�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
t�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
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
v�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.07(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201902250034162019022500341620190225003416202211182138032022111821380320221118213803201902260022572019022600225720190226002257  JA  ARFMdecpA19c                                                                20190213003636  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20190212153708  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20190212153710  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20190212153710  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20190212153711  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20190212153711  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20190212153711  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20190212153711  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20190212153711  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20190212153712                      G�O�G�O�G�O�                JA  ARUP                                                                        20190212155553                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20190212153412  CV  JULD            G�O�G�O�F�8�                JM  ARCAJMQC2.0                                                                 20190224153416  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20190224153416  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20190225152257  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200114231519                      G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20221118123803  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20221123111508                      G�O�G�O�G�O�                