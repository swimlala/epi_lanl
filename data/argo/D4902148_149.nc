CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       ~2018-09-15T15:36:20Z creation;2018-09-15T15:36:23Z conversion to V3.1;2019-12-18T07:20:12Z update;2022-11-21T05:30:09Z update;     
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
_FillValue                 �  ]d   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  aP   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �,   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �\   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ޴   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �D   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �D   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �D   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �D   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �$   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �(   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �8   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �<   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �@   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �DArgo profile    3.1 1.2 19500101000000  20180915153620  20221123111507  4902148 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I1_0397_149                     2C  Dd1NAVIS_A                         0397                            ARGO 011514                     863 @؁���? 1   @؁���>�@<��1'�d1&�x�1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @333@�  @�  @���AffA@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(��B/��B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)y�D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D���D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111)@.�R@{�@�@��\AG�A>�HA^�HA~�HA�p�A�p�A�p�A�p�A�p�A�p�A�p�A�p�B�RB�RB�RB�RB(�B/Q�B7�RB?�RBG�RBO�RBW�RB_�RBg�RBo�RBw�RB�RB��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)C�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
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
��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D {�D ��D!{�D!��D"{�D"��D#{�D#��D${�D$��D%{�D%��D&{�D&��D'{�D'��D({�D(��D)uD)��D*{�D*��D+{�D+��D,{�D,��D-{�D-��D.{�D.��D/{�D/��D0{�D0��D1{�D1��D2{�D2��D3{�D3��D4{�D4��D5{�D5��D6{�D6��D7{�D7��D8{�D8��D9{�D9��D:{�D:��D;{�D;��D<{�D<��D={�D=��D>{�D>��D?{�D?��D@{�D@��DA{�DA��DB{�DB��DC{�DC��DD{�DD��DE{�DE��DF{�DF��DG{�DG��DH{�DH��DI{�DI��DJ{�DJ��DK{�DK��DL{�DL��DM{�DM��DN{�DN��DO{�DO��DP{�DP��DQ{�DQ��DR{�DR��DS{�DS��DT{�DT��DU{�DU��DV{�DV��DW{�DW��DX{�DX��DY{�DY��DZ{�DZ��D[{�D[��D\{�D\��D]{�D]��D^{�D^��D_{�D_��D`{�D`��Da{�Da��Db{�Db��Dc{�Dc��Dd{�Dd��De{�De��Df{�Df��Dg{�Dg��Dh{�Dh��Di{�Di��Dj{�Dj��Dk{�Dk��Dl{�Dl��Dm{�Dm��Dn{�Dn��Do{�Do��Dp{�Dp��Dq{�Dq��Dr{�Dr��Ds{�Ds��Dt{�Dt��Du{�Du��Dv{�Dv��Dw{�Dw��Dx{�Dx��Dy{�Dy��Dz{�Dz��D{{�D{��D|{�D|��D}{�D}��D~{�D~��D{�D��D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D���D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D½�D���D�=�D�}�Dý�D���D�=�D�}�DĽ�D���D�=�D�}�DŽ�D���D�=�D�}�Dƽ�D���D�=�D�}�Dǽ�D���D�=�D�}�DȽ�D���D�=�D�}�Dɽ�D���D�=�D�}�Dʽ�D���D�=�D�}�D˽�D���D�=�D�}�D̽�D���D�=�D�}�Dͽ�D���D�=�D�}�Dν�D���D�=�D�}�DϽ�D���D�=�D�}�Dн�D���D�=�D�}�Dѽ�D���D�=�D�}�Dҽ�D���D�=�D�}�Dӽ�D���D�=�D�}�DԽ�D���D�=�D�}�Dս�D���D�=�D�}�Dֽ�D���D�=�D�}�D׽�D���D�=�D�}�Dؽ�D���D�=�D�}�Dٽ�D���D�=�D�}�Dڽ�D���D�=�D�}�D۽�D���D�=�D�}�Dܽ�D���D�=�D�}�Dݽ�D���D�=�D�}�D޽�D���D�=�D�}�D߽�D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D���D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D��\D��\1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111�A�S�A�S�A�VA�^5A�dZA�jA�ffA�bNA�ffA�`BA�p�A�v�A�x�A�z�A�|�A�~�A�~�A�z�A�|�A�S�A�ȴA��A�v�A���A�?}A�S�A�z�A���A�"�A��A���A��DA�z�A�A���A�7LA��PA�JA�33A�ȴA���A�"�A��9A�~�A��hA��A��\A���A�r�A�O�A�A��A�G�A��A���A��A��A���A�v�A�C�A��
A�ffA�C�A��A�Q�A��TA���A�ZA�\)A�VA�JA�9XA�;dA��A���A�z�A���A��;A�7LA��HA�`BA���A���A�1'A��RA�"�A��hA�+A��9A���A�M�A��AG�A|��Ax�yAv-Au�As�;Ar�9AqS�Ap1'Ao/AnbNAmVAj^5Ah�/Ag��Ag�FAgl�Af�RAe�wAe"�Ad��Ad5?AcoAb�\Aa��Aa|�AaA`{A^�yA^  A]%A\1A[dZAZ�9AY�AX�\AW��AV�HAV�AU�mAU"�AT~�AS��AR�/AR-AQ�-AQVAPz�AO�mAO;dANVAM�wAMt�AMC�AL��AK"�AJffAIoAF��AF(�AE�AD��AD��AD=qAB��ABZAB-AA��A@�yA?�A>ZA=�A=�A<�!A;��A:~�A:A9��A9C�A8{A7��A7C�A7
=A6��A5��A4�RA2�A1XA/A/XA.��A.Q�A.�A.bA-�A-��A+��A(�A(��A(��A'��A't�A&A�A%��A%`BA%33A%�A$��A$ZA$1'A$$�A$�A#��A#�A"jA!�A!�A �A�A�A�A�`AjA�;AC�A�^A�+A�At�A��Av�AI�A�hA�HA�A�#AhsAAZA�AA��A
�!A	��A	`BA	7LA	�A�`A�RAA�A��AhsAM�A�FA�A�`A��A�!A9XAdZA�jAJA Q�@�ȴ@�n�@�V@���@�|�@�
=@�Ĝ@�S�@��H@�ȴ@�%@��@�J@�7@�/@��@���@�j@��@�\)@��H@�=q@�7L@�(�@�+@���@�z�@߅@��@��#@ܴ9@ڰ!@��`@�l�@ְ!@�M�@�hs@�  @�t�@�33@�o@�ff@�z�@Η�@�x�@��@�I�@��;@�C�@�E�@�?}@�  @�S�@��@ƸR@�J@ļj@�;d@�
=@�=q@�O�@�ȴ@�I�@�C�@�o@��@���@���@��+@�@��@�1'@���@���@��@�S�@��R@��@���@��;@��H@��^@���@��
@��R@�{@�p�@�7L@��9@�z�@��@���@�o@�ff@��@��-@�X@�/@�Ĝ@�(�@�b@��m@�
=@���@��+@���@��^@�A�@�K�@�J@��-@��@�%@�%@�r�@�t�@�
=@�~�@���@�&�@��@� �@���@���@�1@��
@�t�@�\)@�S�@�K�@�K�@�33@��@�-@�V@�Q�@�t�@��+@�V@�@���@��-@���@�@���@�X@�V@���@��@�Q�@� �@��P@�33@���@��^@�x�@�%@�Z@�Q�@�9X@���@���@�l�@���@���@�-@���@���@�r�@�Q�@�I�@� �@���@��
@��@�33@��!@�5?@�@��^@���@�G�@�V@���@���@��j@��u@�A�@��m@���@��
@��w@��@�K�@��H@���@�n�@�E�@�J@���@���@�z�@�Q�@��@\)@
=@~�+@}��@}V@|��@|(�@{�
@{��@{dZ@{"�@z�@z�H@z�@{@z�@z��@z�@y�@y�#@y�7@w�;@v��@v��@vȴ@vȴ@v�R@v�R@v�R@vff@u�-@u�@t��@t��@t�D@tz�@tz�@tz�@tj@tI�@t(�@t1@s�
@s@r�\@q��@q��@q�7@qhs@q%@p�9@pr�@p1'@o��@o�@o�P@ol�@o\)@n�@nff@n@m��@m@mp�@l�@l��@l��@lj@l(�@k�F@kdZ@kS�@j�H@i��@i�^@iX@h�`@h�u@hA�@h1'@g�w@gK�@f��@f�R@f5?@e�-@d��@d�D@d�D@d�D@dz�@d�D@d��@d��@d�D@c��@cS�@c33@c@b�@ax�@`��@`�u@`A�@_�@_�P@_|�@_
=@^v�@]�T@]��@]?}@\�@[t�@[S�@[C�@[@Z��@Z^5@Z-@Y��@Y�#@Y��@Y��@Y��@Y��@Y��@Yx�@Y�@X��@XĜ@X��@X�u@Xr�@XA�@X �@W�@W��@WK�@W
=@V��@V�y@Vȴ@V�R@V��@Vv�@V@U�h@U�@Tz�@TZ@T(�@S�
@Sƨ@S��@S�@SdZ@S33@R��@R�!@R~�@Qx�@PA�@P  @O�w@O\)@O�@Nȴ@Nv�@N$�@M�T@M@M��@M�h@M�@Mp�@M`B@MO�@M?}@M�@L�/@L��@LZ@L�@L1@K��@K��@K�
@Kƨ@K��@K"�@J�\@JM�@I�@I��@I&�@H�9@H �@G�@G;d@Fȴ@F��@Fv�@F$�@F@E��@E�-@E�-@E�-@E�h@Ep�@E?}@D�/@D�D@DZ@D9X@D�@C��@CC�@B��@B~�@B^5@BM�@B�@A��@A��@AX@@Ĝ@@r�@@Q�@@A�@@A�@@ �@@b@?|�@>�@>�R@>�+@>5?@=�@=�T@=��@=@=@=�h@=O�@=�@<�/@<��@<j@<1@;o@:�\@:~�@:n�@:�@9�#@9�#@9��@9�^@9�7@9�7@9�7@9x�@9%@81'@8b@7�@7��@7��@7l�@7\)@7+@6�y@6ȴ@6��@6E�@5�@5�-@5O�@4��@4I�@3�
@3��@3�@3C�@2��@2J@1��@1��@1�@1&�@0bN@0A�@0 �@/�@/�@/l�@/K�@/�@/
=@/
=@/
=@/
=@/
=@/
=@.�@.5?@-�T@-`B@,��@,Z@,(�@+ƨ@+C�@+"�@*��@*^5@)��@)��@)hs@)&�@(�u@(  @'|�@'�@&�@&ȴ@&��@&ff@&5?@&$�@%@%��@%/@$�@$I�@$9X@#ƨ@#��@#t�@#dZ@#dZ@#dZ@#S�@#C�@#C�@#o@#@"��@"�\@"=q@"-@!�@!�^@!7L@ �`@ Q�@�;@�w@��@\)@�@V@5?@@@�T@��@@�h@`B@?}@/@V@��@�@��@��@I�@�@1@1@ƨ@dZ@��@�#@x�@X@�@��@��@�9@�@b@�w@K�@
=@��@�y@�R@v�@{@��@�-@�@O�@/@/@��@�j@��@z�@j@I�@9X@9X@9X@(�@�@�@��@ƨ@C�@o@�H@��@��@�\@M�@��@�@��@��@��@X@&�@%@��@�`@��@Ĝ@�9@��@r�@A�@1'@b@��@�P@;d@+@+@�@��@{@`B@/@V@��@��@��@��@��@��@Z@�@�@1@��@�F@t�@33@@
�@
�H@
�H@
��@
��@
~�@
�@	��@	��@	��@	hs@��@�9@bN@bN@bN@Q�@1'@�@�@��@�P@l�@\)@;d@+@�@
=@�y@��@�T@@�-@�-@�-@p�@`B@O�@/@�@�j@j@�@�
@�@dZ@C�@o@�@��@��@��@�!@�!@�\@n�@n�@n�@^5@M�@M�@-@J@�@��@��@��@�7@x�@x�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111�A�S�A�S�A�VA�^5A�dZA�jA�ffA�bNA�ffA�`BA�p�A�v�A�x�A�z�A�|�A�~�A�~�A�z�A�|�A�S�A�ȴA��A�v�A���A�?}A�S�A�z�A���A�"�A��A���A��DA�z�A�A���A�7LA��PA�JA�33A�ȴA���A�"�A��9A�~�A��hA��A��\A���A�r�A�O�A�A��A�G�A��A���A��A��A���A�v�A�C�A��
A�ffA�C�A��A�Q�A��TA���A�ZA�\)A�VA�JA�9XA�;dA��A���A�z�A���A��;A�7LA��HA�`BA���A���A�1'A��RA�"�A��hA�+A��9A���A�M�A��AG�A|��Ax�yAv-Au�As�;Ar�9AqS�Ap1'Ao/AnbNAmVAj^5Ah�/Ag��Ag�FAgl�Af�RAe�wAe"�Ad��Ad5?AcoAb�\Aa��Aa|�AaA`{A^�yA^  A]%A\1A[dZAZ�9AY�AX�\AW��AV�HAV�AU�mAU"�AT~�AS��AR�/AR-AQ�-AQVAPz�AO�mAO;dANVAM�wAMt�AMC�AL��AK"�AJffAIoAF��AF(�AE�AD��AD��AD=qAB��ABZAB-AA��A@�yA?�A>ZA=�A=�A<�!A;��A:~�A:A9��A9C�A8{A7��A7C�A7
=A6��A5��A4�RA2�A1XA/A/XA.��A.Q�A.�A.bA-�A-��A+��A(�A(��A(��A'��A't�A&A�A%��A%`BA%33A%�A$��A$ZA$1'A$$�A$�A#��A#�A"jA!�A!�A �A�A�A�A�`AjA�;AC�A�^A�+A�At�A��Av�AI�A�hA�HA�A�#AhsAAZA�AA��A
�!A	��A	`BA	7LA	�A�`A�RAA�A��AhsAM�A�FA�A�`A��A�!A9XAdZA�jAJA Q�@�ȴ@�n�@�V@���@�|�@�
=@�Ĝ@�S�@��H@�ȴ@�%@��@�J@�7@�/@��@���@�j@��@�\)@��H@�=q@�7L@�(�@�+@���@�z�@߅@��@��#@ܴ9@ڰ!@��`@�l�@ְ!@�M�@�hs@�  @�t�@�33@�o@�ff@�z�@Η�@�x�@��@�I�@��;@�C�@�E�@�?}@�  @�S�@��@ƸR@�J@ļj@�;d@�
=@�=q@�O�@�ȴ@�I�@�C�@�o@��@���@���@��+@�@��@�1'@���@���@��@�S�@��R@��@���@��;@��H@��^@���@��
@��R@�{@�p�@�7L@��9@�z�@��@���@�o@�ff@��@��-@�X@�/@�Ĝ@�(�@�b@��m@�
=@���@��+@���@��^@�A�@�K�@�J@��-@��@�%@�%@�r�@�t�@�
=@�~�@���@�&�@��@� �@���@���@�1@��
@�t�@�\)@�S�@�K�@�K�@�33@��@�-@�V@�Q�@�t�@��+@�V@�@���@��-@���@�@���@�X@�V@���@��@�Q�@� �@��P@�33@���@��^@�x�@�%@�Z@�Q�@�9X@���@���@�l�@���@���@�-@���@���@�r�@�Q�@�I�@� �@���@��
@��@�33@��!@�5?@�@��^@���@�G�@�V@���@���@��j@��u@�A�@��m@���@��
@��w@��@�K�@��H@���@�n�@�E�@�J@���@���@�z�@�Q�@��@\)@
=@~�+@}��@}V@|��@|(�@{�
@{��@{dZ@{"�@z�@z�H@z�@{@z�@z��@z�@y�@y�#@y�7@w�;@v��@v��@vȴ@vȴ@v�R@v�R@v�R@vff@u�-@u�@t��@t��@t�D@tz�@tz�@tz�@tj@tI�@t(�@t1@s�
@s@r�\@q��@q��@q�7@qhs@q%@p�9@pr�@p1'@o��@o�@o�P@ol�@o\)@n�@nff@n@m��@m@mp�@l�@l��@l��@lj@l(�@k�F@kdZ@kS�@j�H@i��@i�^@iX@h�`@h�u@hA�@h1'@g�w@gK�@f��@f�R@f5?@e�-@d��@d�D@d�D@d�D@dz�@d�D@d��@d��@d�D@c��@cS�@c33@c@b�@ax�@`��@`�u@`A�@_�@_�P@_|�@_
=@^v�@]�T@]��@]?}@\�@[t�@[S�@[C�@[@Z��@Z^5@Z-@Y��@Y�#@Y��@Y��@Y��@Y��@Y��@Yx�@Y�@X��@XĜ@X��@X�u@Xr�@XA�@X �@W�@W��@WK�@W
=@V��@V�y@Vȴ@V�R@V��@Vv�@V@U�h@U�@Tz�@TZ@T(�@S�
@Sƨ@S��@S�@SdZ@S33@R��@R�!@R~�@Qx�@PA�@P  @O�w@O\)@O�@Nȴ@Nv�@N$�@M�T@M@M��@M�h@M�@Mp�@M`B@MO�@M?}@M�@L�/@L��@LZ@L�@L1@K��@K��@K�
@Kƨ@K��@K"�@J�\@JM�@I�@I��@I&�@H�9@H �@G�@G;d@Fȴ@F��@Fv�@F$�@F@E��@E�-@E�-@E�-@E�h@Ep�@E?}@D�/@D�D@DZ@D9X@D�@C��@CC�@B��@B~�@B^5@BM�@B�@A��@A��@AX@@Ĝ@@r�@@Q�@@A�@@A�@@ �@@b@?|�@>�@>�R@>�+@>5?@=�@=�T@=��@=@=@=�h@=O�@=�@<�/@<��@<j@<1@;o@:�\@:~�@:n�@:�@9�#@9�#@9��@9�^@9�7@9�7@9�7@9x�@9%@81'@8b@7�@7��@7��@7l�@7\)@7+@6�y@6ȴ@6��@6E�@5�@5�-@5O�@4��@4I�@3�
@3��@3�@3C�@2��@2J@1��@1��@1�@1&�@0bN@0A�@0 �@/�@/�@/l�@/K�@/�@/
=@/
=@/
=@/
=@/
=@/
=@.�@.5?@-�T@-`B@,��@,Z@,(�@+ƨ@+C�@+"�@*��@*^5@)��@)��@)hs@)&�@(�u@(  @'|�@'�@&�@&ȴ@&��@&ff@&5?@&$�@%@%��@%/@$�@$I�@$9X@#ƨ@#��@#t�@#dZ@#dZ@#dZ@#S�@#C�@#C�@#o@#@"��@"�\@"=q@"-@!�@!�^@!7L@ �`@ Q�@�;@�w@��@\)@�@V@5?@@@�T@��@@�h@`B@?}@/@V@��@�@��@��@I�@�@1@1@ƨ@dZ@��@�#@x�@X@�@��@��@�9@�@b@�w@K�@
=@��@�y@�R@v�@{@��@�-@�@O�@/@/@��@�j@��@z�@j@I�@9X@9X@9X@(�@�@�@��@ƨ@C�@o@�H@��@��@�\@M�@��@�@��@��@��@X@&�@%@��@�`@��@Ĝ@�9@��@r�@A�@1'@b@��@�P@;d@+@+@�@��@{@`B@/@V@��@��@��@��@��@��@Z@�@�@1@��@�F@t�@33@@
�@
�H@
�H@
��@
��@
~�@
�@	��@	��@	��@	hs@��@�9@bN@bN@bN@Q�@1'@�@�@��@�P@l�@\)@;d@+@�@
=@�y@��@�T@@�-@�-@�-@p�@`B@O�@/@�@�j@j@�@�
@�@dZ@C�@o@�@��@��@��@�!@�!@�\@n�@n�@n�@^5@M�@M�@-@J@�@��@��@��@�7@x�@x�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111Bq�Bq�Bq�Bq�Bq�Bq�Bq�Bq�Bq�Br�Bq�Bq�Bq�Bq�Br�Bq�Bq�Br�Br�Bu�B�PB��B��B�B�3B�XB�^B�^B�XB�LB�9B�B�B��B��B��B��B��B�uB�DB�B|�Bs�BjBcTB[#BO�BF�BD�BB�B>wB33B,B%�B�BJB��B��B��B��B�B�yB�BɺB��B�bB�JB�Bu�Be`BYBQ�B?}B33B/B(�B�BbB+BB
��B
�B
�B
�sB
��B
�wB
�?B
�B
��B
��B
��B
��B
�1B
v�B
`BB
O�B
I�B
A�B
:^B
2-B
+B
%�B
�B
{B
B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�HB	�/B	�B	��B	��B	��B	��B	ȴB	B	�qB	�XB	�?B	�B	��B	��B	��B	��B	��B	�uB	�bB	�DB	�+B	�B	�B	}�B	y�B	v�B	s�B	n�B	l�B	jB	iyB	e`B	^5B	YB	Q�B	I�B	E�B	B�B	@�B	>wB	:^B	49B	2-B	1'B	.B	(�B	!�B	�B	�B	�B	oB	JB	%B	B	B��B��B��B��B��B�B�B�fB�)B��B��B��BɺBǮBƨBŢBĜBB�dB�?B�9B�3B�!B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�uB�bB�PB�=B�1B�%B�B}�By�Bu�Br�Bp�Bo�Bn�Bk�BgmBdZBbNBaHB_;B]/B[#BYBVBR�BQ�BP�BP�BO�BO�BN�BL�BK�BI�BH�BF�BE�BE�BD�BC�BB�B@�B>wB;dB9XB9XB8RB7LB5?B2-B0!B.B.B-B,B+B)�B(�B(�B&�B%�B%�B%�B%�B$�B$�B$�B#�B#�B"�B!�B�B �B�B �B �B�B�B �B �B�B�B �B �B �B�B�B�B�B �B�B!�B$�B#�B"�B!�B!�B!�B �B �B�B�B�B�B�B�B�B$�B(�B,B,B-B0!B1'B0!B2-B49B49B6FB7LB6FB7LB7LB8RB8RB8RB9XB9XB:^B;dB<jB=qB>wBE�BH�BP�BS�BS�BS�BS�BS�BT�BVBW
BYBYBZBcTBcTBgmBjBjBn�Bq�Bu�Bu�Bu�Bv�Bv�Bv�By�Bz�B|�B� B�B�B�+B�%B�VB�{B�{B�uB�uB�uB�uB�uB�uB�oB�oB��B��B��B��B�B�9B�RB�jB�}B��BÖBǮBɺB��B��B��B��B�B�B�#B�TB�`B�mB�yB�B�B�B�B�B��B��B��B��B	B	%B	+B	+B	+B	+B	+B	1B	JB	hB	{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	!�B	#�B	$�B	&�B	(�B	,B	.B	.B	/B	/B	0!B	1'B	33B	2-B	49B	6FB	7LB	8RB	:^B	=qB	>wB	A�B	C�B	D�B	D�B	D�B	E�B	F�B	F�B	H�B	I�B	J�B	K�B	L�B	N�B	P�B	T�B	ZB	]/B	]/B	]/B	]/B	]/B	^5B	aHB	dZB	hsB	m�B	p�B	r�B	r�B	s�B	s�B	s�B	s�B	t�B	t�B	t�B	w�B	x�B	z�B	{�B	|�B	|�B	}�B	~�B	� B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�%B	�%B	�+B	�7B	�7B	�=B	�=B	�DB	�VB	�\B	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�'B	�-B	�-B	�?B	�LB	�XB	�^B	�dB	�qB	�wB	�wB	�wB	��B	ÖB	ŢB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�
B	�
B	�B	�B	�B	�B	�B	�#B	�#B	�)B	�)B	�)B	�/B	�/B	�5B	�;B	�BB	�BB	�BB	�BB	�HB	�HB	�NB	�NB	�NB	�NB	�TB	�ZB	�ZB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
%B
+B
	7B

=B

=B

=B
DB
DB
JB
JB
JB
JB
JB
PB
PB
VB
VB
VB
\B
\B
bB
oB
uB
uB
uB
uB
uB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
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
"�B
"�B
"�B
#�B
$�B
%�B
%�B
%�B
&�B
&�B
&�B
'�B
'�B
'�B
(�B
(�B
)�B
)�B
+B
+B
,B
.B
.B
.B
.B
/B
/B
0!B
0!B
1'B
2-B
2-B
33B
33B
33B
33B
33B
33B
49B
49B
49B
5?B
6FB
7LB
8RB
8RB
8RB
9XB
:^B
:^B
:^B
;dB
;dB
<jB
<jB
<jB
>wB
?}B
?}B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
F�B
F�B
G�B
H�B
H�B
H�B
H�B
I�B
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
M�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
S�B
T�B
T�B
T�B
T�B
T�B
T�B
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
YB
YB
YB
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
ZB
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
^5B
^5B
_;B
_;B
_;B
_;B
`BB
`BB
`BB
`BB
`BB
aHB
bNB
cTB
cTB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
e`B
e`B
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
gmB
gmB
gmB
hsB
hsB
iyB
iyB
iyB
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
l�B
l�B
m�B
m�B
m�B
m�B
m�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
p�B
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
s�B
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
u�B
u�B
u�B
u�B
u�B
u�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111Bq�Bq�Bq�Bq�Bq�Bq�Bq�Bq�Bq�Br�Bq�Bq�Bq�Bq�Br�Bq�Bq�BsMBt�B|�B�dB��B�B��B�XB�]B��B��B��B��B��B�hB�B�sB�:B�BB�OB��B�SB�PB�zB�Bv`Bl�Be,B]�BQ�BG+BE9BC�B@�B4�B-wB(sB�B(B��B�XB��B�B�AB�qBݲB�BB��B��B��B��BxRBh>B[=BTBA;B4�B0�B*�B�B�B1BGB
�B
��B
�vB
�B
�B
��B
�`B
�;B
��B
�
B
��B
�kB
��B
z�B
c:B
QhB
KDB
C-B
<B
3�B
,WB
'RB
!�B
YB
�B	��B	�FB	�ZB	�B	�B	�]B	�!B	��B	�B	�B	�B	֡B	��B	�2B	�aB	�B	�	B	��B	�]B	��B	�B	��B	�0B	��B	�\B	��B	��B	�{B	��B	�dB	�B	��B	��B	~�B	z�B	w�B	t�B	oiB	mB	kB	j�B	gB	_�B	[	B	TFB	J�B	F�B	CaB	AB	?}B	<B	4�B	2�B	2B	/iB	*�B	#:B	�B	_B	SB	�B	�B	�B	�B	�B	 OB�xB�RB�`B��B�B�B��B�B��BϑB�~B�XB�B��B�?BŢB�B�wB��B��B�9B�'B��B��B�_B�_B�>B��B�LB�,B�B�,B�tB��B��B��B��B�B�sB��B��B��B�)B�B�_B�9B�B|6BwLBs�Bq[Bp;Bo�Bl�Bi�Be�BcBbB`BB^jB\CBZ�BW�BT,BR�BQ4BQNBPbBPbBO�BM�BL~BKDBI�BG�BF%BFBEBD�BC�BA�B?�B=qB:xB9�B8�B8B7LB4�B1�B/ B.}B-�B-]B,=B*�B)yB)�B(XB'8B&LB&fB&fB%`B%zB%�B$�B$�B#�B"�B �B!|B �B!�B"4B �B �B!|B!HB �B �B!-B!B!-B �B 'B�B �B!-B \B"NB%�B$�B#�B"�B"NB"4B!-B!�B�B�B!B�B�BpB!HB%zB)*B,=B,"B-]B0�B1�B0�B2�B4nB4�B6zB7�B6�B88B8B9	B9$B9>B:*B:DB;B;�B<�B=�B>�BE�BH�BQhBT�BT�BTaBTaBTFBUMBVmBWsBYKBYeBZ�BcnBc�Bh
BkBk�BoiBr|BvBv+Bv+BwBwLBw�Bz^B{dB}�B��B��B��B��B��B�vB��B��B��B��B��B��B��B��B�&B�[B�7B�\B�tB�0B��B�TB�lB�jB��B��B��B��B�	B�0B�B�.B�hB�SBؓB��B�B��B��B�B��B�B��B�AB�3B��B�8B�^B�qB	uB	?B	EB	_B	_B	_B	_B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	B	�B	B	�B	 �B	!�B	$B	%,B	'8B	)_B	,=B	.IB	.IB	/iB	/�B	0�B	1�B	3hB	2|B	4nB	6zB	7�B	8�B	:�B	=�B	>�B	A�B	C�B	D�B	D�B	D�B	E�B	F�B	F�B	H�B	I�B	J�B	K�B	L�B	O(B	Q�B	UgB	ZB	]IB	]/B	]IB	]IB	]dB	^�B	a�B	d�B	h�B	m�B	p�B	r�B	r�B	s�B	s�B	s�B	s�B	t�B	t�B	uB	xB	y	B	z�B	|B	}"B	}<B	~B	B	�4B	�;B	�AB	�AB	�'B	�'B	�AB	�aB	�MB	�9B	�YB	�tB	�_B	�RB	�RB	�XB	�XB	��B	�pB	�vB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�>B	�"B	�)B	�B	�)B	�B	�B	�)B	�)B	�]B	�UB	�[B	�aB	�|B	��B	��B	�rB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	��B	�B	�B	�&B	�B	�B	�B	�B	�?B	�$B	�$B	�EB	�EB	�_B	�KB	�QB	�=B	�=B	�CB	�]B	�CB	�dB	�~B	�jB	�pB	��B	�\B	�\B	�\B	�bB	�|B	�hB	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�0B	�"B	�B
 4B
 B
;B
GB
GB
SB
tB
_B
	lB

XB

XB

XB
xB
xB
dB
JB
~B
dB
dB
�B
jB
pB
pB
pB
vB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
�B
�B
B
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
!B
!�B
"�B
#B
#B
#�B
$�B
%�B
%�B
%�B
'B
'B
'B
($B
($B
($B
)B
)DB
*B
*B
+B
+QB
,WB
./B
.B
./B
.cB
/iB
/5B
0;B
0UB
1AB
2aB
2GB
3MB
33B
3MB
3MB
33B
33B
49B
4TB
4nB
5tB
6zB
7�B
8lB
8�B
8�B
9�B
:xB
:xB
:�B
;B
;�B
<�B
<�B
<�B
>�B
?�B
?�B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
F�B
F�B
G�B
H�B
H�B
H�B
H�B
J	B
K�B
K�B
K�B
K�B
L�B
L�B
MB
MB
MB
M�B
M�B
NB
M�B
M�B
M�B
NB
N�B
N�B
OB
N�B
OB
P.B
PHB
QB
Q B
RB
R B
RB
RB
SB
S&B
S&B
T,B
UB
UB
UB
U2B
U2B
UB
VB
W?B
W$B
W$B
XB
XB
X+B
YKB
Y1B
YKB
YB
YKB
Y1B
YB
YB
YB
Y1B
ZB
ZQB
Z7B
ZQB
[=B
[=B
[=B
[WB
\]B
\CB
\CB
\CB
\CB
\CB
]IB
]IB
]dB
]dB
^5B
^5B
^5B
^OB
^5B
^jB
^OB
^OB
_VB
_pB
_VB
_pB
`\B
`\B
`BB
`\B
`�B
a|B
b�B
cnB
cnB
dZB
dtB
dtB
dtB
dtB
dtB
d�B
ezB
ezB
ezB
ezB
ezB
f�B
f�B
f�B
g�B
gmB
gmB
g�B
g�B
g�B
g�B
h�B
h�B
iyB
i�B
i�B
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
l�B
l�B
m�B
m�B
m�B
m�B
m�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
p�B
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
s�B
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
u�B
u�B
u�B
u�B
u�B
u�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111�@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<��I<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.07(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201809260036332018092600363320180926003633202211182136162022111821361620221118213616201809270018162018092700181620180927001816  JA  ARFMdecpA19c                                                                20180916003514  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180915153620  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180915153621  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180915153622  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180915153622  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180915153622  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180915153622  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180915153622  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180915153623  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180915153623                      G�O�G�O�G�O�                JA  ARUP                                                                        20180915155527                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180915153909  CV  JULD            G�O�G�O�F��                JM  ARCAJMQC2.0                                                                 20180925153633  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180925153633  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180926151816  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200114171529                      G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20221118123616  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20221123111507                      G�O�G�O�G�O�                