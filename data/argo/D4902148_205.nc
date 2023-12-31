CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2020-03-28T15:38:12Z creation;2020-03-28T15:38:18Z conversion to V3.1;2022-11-21T05:27:20Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         BPRIMARY|https://orcid.org/0000-0001-9150-6442|Kanako Sato, JAMSTEC        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7,   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7<   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7@   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    7D   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7T   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7d   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7t   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7|   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8,   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    80   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    84   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     88   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8X   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8\   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8`   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
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
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    9    CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        :    PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  :   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]H   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  a4   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �x   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �d   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �@   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ޘ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �(   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �(   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �(   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �(   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �    HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �$   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �(Argo profile    3.1 1.2 19500101000000  20200328153812  20221123114511  4902148 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I1_0397_205                     2C  D   NAVIS_A                         0397                            ARGO 011514                     863 @��	{B�1   @����Y @:��)^��dw��3�1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D޼�D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�C3D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�C3D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�ɚD��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @���@�ff@�ffA33A?33A_33A33A���A���A���A���Aϙ�Aߙ�AA���B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fC�3C�3C�3C�3C	�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C!�3C#�3C%�3C'�3C)�3C+�3C-�3C/�3C1�3C3�3C5�3C7�3C9�3C;�3C=�3C?�3CA�3CC�3CE�3CG�3CI�3CK�3CM�3CO�3CQ�3CS�3CU�3CW�3CY�3C[�3C]�3C_�3Ca�3Cc�3Ce�3Cg�3Ci�3Ck�3Cm�3Co�3Cq�3Cs�3Cu�3Cw�3Cy�3C{�3C}�3C�3C���C���C���C���C���C���C���C���C���C���C���C���C���C�fC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D |�D ��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D	|�D	��D
|�D
��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D |�D ��D!|�D!��D"|�D"��D#|�D#��D$|�D$��D%|�D%��D&|�D&��D'|�D'��D(|�D(��D)|�D)��D*|�D*��D+|�D+��D,|�D,��D-|�D-��D.|�D.��D/|�D/��D0|�D0��D1|�D1��D2|�D2��D3|�D3��D4|�D4��D5|�D5��D6|�D6��D7|�D7��D8|�D8��D9|�D9��D:|�D:��D;|�D;��D<|�D<��D=|�D=��D>|�D>��D?|�D?��D@|�D@��DA|�DA��DB|�DB��DC|�DC��DD|�DD��DE|�DE��DF|�DF��DG|�DG��DH|�DH��DI|�DI��DJ|�DJ��DK|�DK��DL|�DL��DM|�DM��DN|�DN��DO|�DO��DP|�DP��DQ|�DQ��DR|�DR��DS|�DS��DT|�DT��DU|�DU��DV|�DV��DW|�DW��DX|�DX��DY|�DY��DZ|�DZ��D[|�D[��D\|�D\��D]|�D]��D^|�D^��D_|�D_��D`|�D`��Da|�Da��Db|�Db��Dc|�Dc��Dd|�Dd��De|�De��Df|�Df��Dg|�Dg��Dh|�Dh��Di|�Di��Dj|�Dj��Dk|�Dk��Dl|�Dl��Dm|�Dm��Dn|�Dn��Do|�Do��Dp|�Dp��Dq|�Dq��Dr|�Dr��Ds|�Ds��Dt|�Dt��Du|�Du��Dv|�Dv��Dw|�Dw��Dx|�Dx��Dy|�Dy��Dz|�Dz��D{|�D{��D||�D|��D}|�D}��D~|�D~��D|�D��D�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD¾fD��fD�>fD�~fDþfD��fD�>fD�~fDľfD��fD�>fD�~fDžfD��fD�>fD�~fDƾfD��fD�>fD�~fDǾfD��fD�>fD�~fDȾfD��fD�>fD�~fDɾfD��fD�>fD�~fDʾfD��fD�>fD�~fD˾fD��fD�>fD�~fD̾fD��fD�>fD�~fD;fD��fD�>fD�~fDξfD��fD�>fD�~fDϾfD��fD�>fD�~fDоfD��fD�>fD�~fDѾfD��fD�>fD�~fDҾfD��fD�>fD�~fDӾfD��fD�>fD�~fDԾfD��fD�>fD�~fDվfD��fD�>fD�~fD־fD��fD�>fD�~fD׾fD��fD�>fD�~fDؾfD��fD�>fD�~fDپfD��fD�>fD�~fDھfD��fD�>fD�~fD۾fD��fD�>fD�~fDܾfD��fD�>fD�~fDݾfD��fD�>fD�~fD޻3D��fD�>fD�~fD߾fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�A�D�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�A�D�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD��fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD�� D�� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A���A���A��/A��;A��;A��;A��;A��HA��`A��`A��TA��yA��mA��TA��`A��A�ȴA���A�VA�"�A�bA���A���A��HA���A�/A��TA��
A���A�ƨA���A�dZA�oA�K�A���A��!A�A�A���A�t�A�?}A���A��A��A�r�A���A�jA��\A���A�%A���A�oA��RA���A�7LA�bNA��RA�
=A�t�A�C�A��HA�\)A���A�S�A�1'A��A��+A���A�bNA�ȴA�^5A�I�A�33A���A��A�5?A�hsA��9A�^5A��A��!A�E�A��-A�\)A��9A�A�=qA���A��FA���A�v�A�O�A��
A�"�A���A��A�(�A���A��A�bA�1'A�1A���A~��A}C�A|ȴA|Q�A{�#A{C�AydZAxAv-At=qAr�Aq��Ap��An�yAmK�Ai�hAg�hAgVAfbNAeoAbz�A`��A_�7A_"�A^��A^v�A]�FA\��A[�^AZ1'AY"�AX�!AX  AW&�AV��AVv�AU��AUS�AT$�AR�AR�APffAOhsANQ�AM�mAM�wAMdZAL�AL^5AK
=AI�PAI
=AH{AF��AFE�AE�#AEl�AEK�ADZAC�^AC+AB�!AAA?�mA?|�A?K�A?&�A?
=A>�/A>�9A>Q�A=�
A=S�A<��A;�mA8�yA8bNA7�wA7XA6��A61A4��A3�A2�uA1ƨA0�!A0n�A01A/C�A.��A-�FA,��A,�yA,��A,5?A+A+x�A+�A*��A)�TA)|�A(�!A'33A&jA#�wA"��A"ZA"A�A" �A!��A �DAO�A�jA�AS�A��AhsA��AA��A�7A\)A�RAXA  A?}A��AQ�A�A�PA��A �A�^A\)A�uA%A��A��A
=A5?AA;dA��A^5A��A�A�/A��A�+AZA  A\)A 1'@�?}@�l�@�ȴ@���@�`B@�V@���@���@��
@�+@�&�@� �@�33@�r�@�7L@�t�@�-@�G�@㝲@�hs@�l�@��T@�z�@� �@�ƨ@ۍP@�"�@ف@���@�M�@�hs@���@�hs@�V@�j@ύP@�Q�@�$�@��@ə�@�/@���@ȼj@�z�@�b@Ǯ@�~�@öF@���@�7L@��@�\)@��@���@�@�`B@��@�t�@�{@�hs@�?}@���@��D@�b@���@�C�@�M�@���@�?}@��u@�|�@�5?@��^@�%@�z�@�(�@�"�@�`B@�V@�b@��@���@�V@���@���@�
=@���@��@�(�@�|�@�n�@��-@��@�bN@�1'@��@�"�@��@���@�n�@�J@��7@��@�%@���@�  @���@�l�@��@��\@�{@���@���@�b@���@�S�@��@�=q@��T@�X@���@�j@�1'@��@�
=@��+@�$�@���@�hs@�`B@�O�@��`@�9X@��;@�|�@��H@���@�n�@�{@��@���@���@��@�%@���@�ƨ@�|�@�K�@�33@��@���@��\@���@�X@�j@��@��@��@��@�S�@���@�$�@��h@�?}@��j@��@�9X@��m@��F@�t�@��@���@��+@�V@��@�{@�{@�@��#@��^@��-@��-@���@���@�x�@�G�@��/@���@��u@�z�@�I�@��@l�@~�y@~�+@~{@}p�@}/@|�@|�@|Z@|I�@|(�@{��@{�
@{ƨ@{�F@{��@{C�@{"�@z��@z~�@y��@y%@x��@x�`@x�9@w�w@w;d@w
=@w
=@v��@v��@u@u?}@u/@u?}@uV@t�j@t9X@tZ@t9X@t(�@t�@s"�@r��@r~�@r^5@q�@qhs@p�`@p��@p1'@o��@n�y@nV@m�T@m��@m��@m�@l��@l��@lI�@l(�@l(�@l�@kƨ@kS�@kS�@kS�@j��@i��@i��@ix�@i�7@i�@hbN@hA�@hb@g�P@g+@g
=@f��@f��@fv�@f@e@eO�@d��@d�@d�j@dz�@dI�@cƨ@b�@a��@a&�@`��@`��@`r�@`Q�@`Q�@`1'@_��@_\)@_;d@_
=@^�R@^�+@^ff@^{@]O�@\Z@[�m@[��@Z�H@Z^5@ZM�@Z=q@Z-@Z�@ZJ@Y��@Y�#@Y�^@Y�7@Yx�@Y%@XA�@W�P@V�@VV@U@Up�@U`B@T�/@T9X@T�@S�m@SS�@S@RM�@Q�#@Q�^@Q�7@QG�@P�`@Pr�@O��@O\)@O
=@N�y@N��@NV@NE�@N5?@N@M�-@M`B@MV@L�@L�D@Lj@L(�@K��@K�m@K�F@K��@K"�@J��@J��@J�\@Jn�@J=q@I��@Ihs@HQ�@H1'@Hb@G�;@G�@G�@G�P@G\)@GK�@G;d@G;d@G+@F��@F�R@F��@F��@Fv�@FE�@F$�@F{@F@E�T@E`B@EV@D�D@C��@CS�@B��@Bn�@B�@A��@A��@AX@@�`@@�u@@Q�@@1'@@  @?��@?\)@?
=@>ȴ@>��@>��@>v�@=�@=?}@<��@<��@<�/@<j@<Z@<9X@;�m@;�@;S�@;33@;33@;o@:�@:�!@:~�@:=q@9�#@9��@9��@9hs@9G�@9%@8��@8bN@81'@7�w@7|�@7
=@7
=@6��@6�+@6$�@5��@5�h@5p�@5�@4�@4��@4Z@4(�@41@3�m@3dZ@3o@2�H@2��@2�!@2n�@2-@2J@1��@1�@1�#@1�@1��@1x�@1%@0�9@0r�@0Q�@0A�@0 �@0b@0  @/��@/�@/|�@.��@.v�@-�@-��@-��@-��@-��@-�@-`B@-�@,�@,z�@,I�@,1@+�m@+�F@+��@+t�@+S�@+33@+o@+@*��@*^5@*=q@)��@)��@)��@)��@)��@)�7@)hs@)�@(�`@(�9@(�9@(��@(��@(r�@(1'@'|�@&��@&��@&5?@%�-@%p�@%`B@%V@$�@$Z@$I�@$�@#��@#ƨ@#dZ@#@"�H@"��@"~�@"=q@"�@!�#@!��@!��@!x�@!&�@ ��@ ��@ bN@ 1'@ b@�@��@|�@\)@+@ȴ@�R@v�@ff@{@@?}@��@�@�j@j@(�@��@ƨ@t�@dZ@dZ@dZ@C�@"�@�@��@n�@M�@-@��@�^@x�@7L@�`@��@Ĝ@��@r�@r�@r�@A�@�@�w@�P@K�@
=@�y@��@v�@5?@@��@@��@�h@�@�@�D@1@S�@33@�@��@�!@�\@~�@~�@~�@~�@~�@~�@~�@~�@~�@~�@~�@J@��@�@��@X@��@�9@Q�@A�@  @��@�w@�w@�P@K�@;d@;d@+@
=@�y@�@��@��@v�@V@{@�@��@p�@p�@`B@`B@O�@/@V@��@�@��@�D@9X@(�@(�@�@��@�
@t�@33@o@@
�H@
��@
��@
n�@
n�@
^5@
^5@
M�@
�@	�@	��@	x�@	hs@	G�@	&�@	�@�`@��@�u@r�@bN@Q�@A�@A�@A�@ �@��@��@|�@;d@��@�@ȴ@�R@��@��@��@v�@v�@v�@5?@{@@��@�h@�@�@�@z�@9X@��@��@t�@dZ@33@@�H@��@�!@��@~�@n�@M�@-@��@�#@�#@�^@X@X1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A���A���A��/A��;A��;A��;A��;A��HA��`A��`A��TA��yA��mA��TA��`A��A�ȴA���A�VA�"�A�bA���A���A��HA���A�/A��TA��
A���A�ƨA���A�dZA�oA�K�A���A��!A�A�A���A�t�A�?}A���A��A��A�r�A���A�jA��\A���A�%A���A�oA��RA���A�7LA�bNA��RA�
=A�t�A�C�A��HA�\)A���A�S�A�1'A��A��+A���A�bNA�ȴA�^5A�I�A�33A���A��A�5?A�hsA��9A�^5A��A��!A�E�A��-A�\)A��9A�A�=qA���A��FA���A�v�A�O�A��
A�"�A���A��A�(�A���A��A�bA�1'A�1A���A~��A}C�A|ȴA|Q�A{�#A{C�AydZAxAv-At=qAr�Aq��Ap��An�yAmK�Ai�hAg�hAgVAfbNAeoAbz�A`��A_�7A_"�A^��A^v�A]�FA\��A[�^AZ1'AY"�AX�!AX  AW&�AV��AVv�AU��AUS�AT$�AR�AR�APffAOhsANQ�AM�mAM�wAMdZAL�AL^5AK
=AI�PAI
=AH{AF��AFE�AE�#AEl�AEK�ADZAC�^AC+AB�!AAA?�mA?|�A?K�A?&�A?
=A>�/A>�9A>Q�A=�
A=S�A<��A;�mA8�yA8bNA7�wA7XA6��A61A4��A3�A2�uA1ƨA0�!A0n�A01A/C�A.��A-�FA,��A,�yA,��A,5?A+A+x�A+�A*��A)�TA)|�A(�!A'33A&jA#�wA"��A"ZA"A�A" �A!��A �DAO�A�jA�AS�A��AhsA��AA��A�7A\)A�RAXA  A?}A��AQ�A�A�PA��A �A�^A\)A�uA%A��A��A
=A5?AA;dA��A^5A��A�A�/A��A�+AZA  A\)A 1'@�?}@�l�@�ȴ@���@�`B@�V@���@���@��
@�+@�&�@� �@�33@�r�@�7L@�t�@�-@�G�@㝲@�hs@�l�@��T@�z�@� �@�ƨ@ۍP@�"�@ف@���@�M�@�hs@���@�hs@�V@�j@ύP@�Q�@�$�@��@ə�@�/@���@ȼj@�z�@�b@Ǯ@�~�@öF@���@�7L@��@�\)@��@���@�@�`B@��@�t�@�{@�hs@�?}@���@��D@�b@���@�C�@�M�@���@�?}@��u@�|�@�5?@��^@�%@�z�@�(�@�"�@�`B@�V@�b@��@���@�V@���@���@�
=@���@��@�(�@�|�@�n�@��-@��@�bN@�1'@��@�"�@��@���@�n�@�J@��7@��@�%@���@�  @���@�l�@��@��\@�{@���@���@�b@���@�S�@��@�=q@��T@�X@���@�j@�1'@��@�
=@��+@�$�@���@�hs@�`B@�O�@��`@�9X@��;@�|�@��H@���@�n�@�{@��@���@���@��@�%@���@�ƨ@�|�@�K�@�33@��@���@��\@���@�X@�j@��@��@��@��@�S�@���@�$�@��h@�?}@��j@��@�9X@��m@��F@�t�@��@���@��+@�V@��@�{@�{@�@��#@��^@��-@��-@���@���@�x�@�G�@��/@���@��u@�z�@�I�@��@l�@~�y@~�+@~{@}p�@}/@|�@|�@|Z@|I�@|(�@{��@{�
@{ƨ@{�F@{��@{C�@{"�@z��@z~�@y��@y%@x��@x�`@x�9@w�w@w;d@w
=@w
=@v��@v��@u@u?}@u/@u?}@uV@t�j@t9X@tZ@t9X@t(�@t�@s"�@r��@r~�@r^5@q�@qhs@p�`@p��@p1'@o��@n�y@nV@m�T@m��@m��@m�@l��@l��@lI�@l(�@l(�@l�@kƨ@kS�@kS�@kS�@j��@i��@i��@ix�@i�7@i�@hbN@hA�@hb@g�P@g+@g
=@f��@f��@fv�@f@e@eO�@d��@d�@d�j@dz�@dI�@cƨ@b�@a��@a&�@`��@`��@`r�@`Q�@`Q�@`1'@_��@_\)@_;d@_
=@^�R@^�+@^ff@^{@]O�@\Z@[�m@[��@Z�H@Z^5@ZM�@Z=q@Z-@Z�@ZJ@Y��@Y�#@Y�^@Y�7@Yx�@Y%@XA�@W�P@V�@VV@U@Up�@U`B@T�/@T9X@T�@S�m@SS�@S@RM�@Q�#@Q�^@Q�7@QG�@P�`@Pr�@O��@O\)@O
=@N�y@N��@NV@NE�@N5?@N@M�-@M`B@MV@L�@L�D@Lj@L(�@K��@K�m@K�F@K��@K"�@J��@J��@J�\@Jn�@J=q@I��@Ihs@HQ�@H1'@Hb@G�;@G�@G�@G�P@G\)@GK�@G;d@G;d@G+@F��@F�R@F��@F��@Fv�@FE�@F$�@F{@F@E�T@E`B@EV@D�D@C��@CS�@B��@Bn�@B�@A��@A��@AX@@�`@@�u@@Q�@@1'@@  @?��@?\)@?
=@>ȴ@>��@>��@>v�@=�@=?}@<��@<��@<�/@<j@<Z@<9X@;�m@;�@;S�@;33@;33@;o@:�@:�!@:~�@:=q@9�#@9��@9��@9hs@9G�@9%@8��@8bN@81'@7�w@7|�@7
=@7
=@6��@6�+@6$�@5��@5�h@5p�@5�@4�@4��@4Z@4(�@41@3�m@3dZ@3o@2�H@2��@2�!@2n�@2-@2J@1��@1�@1�#@1�@1��@1x�@1%@0�9@0r�@0Q�@0A�@0 �@0b@0  @/��@/�@/|�@.��@.v�@-�@-��@-��@-��@-��@-�@-`B@-�@,�@,z�@,I�@,1@+�m@+�F@+��@+t�@+S�@+33@+o@+@*��@*^5@*=q@)��@)��@)��@)��@)��@)�7@)hs@)�@(�`@(�9@(�9@(��@(��@(r�@(1'@'|�@&��@&��@&5?@%�-@%p�@%`B@%V@$�@$Z@$I�@$�@#��@#ƨ@#dZ@#@"�H@"��@"~�@"=q@"�@!�#@!��@!��@!x�@!&�@ ��@ ��@ bN@ 1'@ b@�@��@|�@\)@+@ȴ@�R@v�@ff@{@@?}@��@�@�j@j@(�@��@ƨ@t�@dZ@dZ@dZ@C�@"�@�@��@n�@M�@-@��@�^@x�@7L@�`@��@Ĝ@��@r�@r�@r�@A�@�@�w@�P@K�@
=@�y@��@v�@5?@@��@@��@�h@�@�@�D@1@S�@33@�@��@�!@�\@~�@~�@~�@~�@~�@~�@~�@~�@~�@~�@~�@J@��@�@��@X@��@�9@Q�@A�@  @��@�w@�w@�P@K�@;d@;d@+@
=@�y@�@��@��@v�@V@{@�@��@p�@p�@`B@`B@O�@/@V@��@�@��@�D@9X@(�@(�@�@��@�
@t�@33@o@@
�H@
��@
��@
n�@
n�@
^5@
^5@
M�@
�@	�@	��@	x�@	hs@	G�@	&�@	�@�`@��@�u@r�@bN@Q�@A�@A�@A�@ �@��@��@|�@;d@��@�@ȴ@�R@��@��@��@v�@v�@v�@5?@{@@��@�h@�@�@�@z�@9X@��@��@t�@dZ@33@@�H@��@�!@��@~�@n�@M�@-@��@�#@�#@�^@X@X1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BQ�BQ�BQ�BQ�BQ�BQ�BQ�BQ�BQ�BQ�BQ�BP�BP�BP�BM�Bp�B��B��B�'B�9B�9B�9B�9B�?B�LB�RB�XB�XB�XB�XB�RB�LB�3B�'B��B��B��B�\B�VB�=B}�Bz�Bz�Bz�B{�By�Bu�Bl�B`BBdZBffB\)BE�B/B+B#�B�BbB+BB��B��B�TB�/B�B�
B��BȴB��B�XB�3B�3B�'B��B��B�bBv�Bl�BffBaHB[#BS�BL�BF�B=qB0!B'�B�B{BuBbBPB%B
��B
��B
�B
�B
�mB
��B
��B
�qB
�B
��B
�B
z�B
v�B
s�B
o�B
jB
]/B
R�B
E�B
9XB
.B
&�B
 �B
{B
+B	�B	�fB	�TB	�5B	�B	ǮB	�wB	�LB	�?B	�3B	�'B	�B	��B	��B	��B	�{B	�hB	�VB	�=B	�1B	�%B	�B	� B	w�B	p�B	k�B	bNB	]/B	[#B	[#B	[#B	[#B	ZB	YB	S�B	N�B	J�B	F�B	A�B	?}B	<jB	:^B	9XB	5?B	33B	0!B	-B	%�B	 �B	�B	�B	�B	�B	�B	�B	�B	{B	bB	PB	+B��B�B�B�B�yB�ZB�;B�B�
B��B��B��B��BȴBƨBĜB��B��B��B�wB�qB�dB�^B�RB�FB�9B�!B��B��B��B�uB�oB�hB�bB�\B�=B�%B�B�B~�By�Bt�Br�Bq�Bq�Bp�Bo�Bl�BhsBdZBbNB`BB_;B\)BYBW
BT�BR�BP�BN�BK�BG�BE�BC�BB�BA�B@�B?}B>wB=qB=qB<jB;dB;dB:^B9XB8RB5?B49B2-B2-B1'B1'B0!B.B-B-B,B+B)�B(�B&�B&�B%�B%�B$�B#�B!�B �B �B!�B �B �B �B�B�B�B�B�B �B �B �B�B�B �B"�B"�B"�B#�B#�B$�B#�B#�B#�B$�B'�B'�B&�B&�B(�B)�B)�B+B,B.B0!B33B49B49B49B5?B6FB7LB7LB9XB:^B:^B;dB=qB>wBC�BC�BE�BE�BI�BP�BP�BR�BVBXBZBZBZBaHBdZBgmBiyBk�Bm�Bp�Bv�Bw�Bw�By�B{�B|�B}�B~�B� B�B�B�B�B�7B�=B�=B�JB�VB�\B�\B�oB��B��B��B��B��B��B��B�B�B�B�!B�FB�RB�^B�qB�wB�wB�wB��BŢBƨBȴB��B��B��B��B��B��B�B�
B�B�)B�NB�ZB�`B�fB�fB�mB�sB�B�B��B��B��B��B��B��B	  B	B		7B	DB	bB	oB	{B	�B	�B	�B	�B	"�B	$�B	%�B	(�B	-B	/B	0!B	1'B	2-B	2-B	2-B	2-B	33B	5?B	9XB	=qB	?}B	?}B	?}B	@�B	D�B	E�B	I�B	K�B	M�B	P�B	Q�B	S�B	S�B	T�B	T�B	T�B	VB	VB	W
B	W
B	W
B	XB	XB	XB	ZB	^5B	`BB	aHB	bNB	ffB	iyB	iyB	jB	l�B	n�B	p�B	s�B	u�B	u�B	u�B	u�B	x�B	z�B	{�B	|�B	|�B	|�B	~�B	�B	�B	�B	�B	�%B	�+B	�1B	�7B	�JB	�VB	�bB	�hB	�hB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�'B	�-B	�-B	�3B	�9B	�9B	�FB	�RB	�jB	�wB	�wB	�}B	��B	��B	��B	��B	B	ÖB	ÖB	ĜB	ŢB	ŢB	ƨB	ǮB	ɺB	��B	��B	��B	��B	��B	�B	�B	�B	�B	�
B	�
B	�
B	�B	�B	�B	�B	�#B	�)B	�5B	�;B	�BB	�HB	�HB	�NB	�ZB	�ZB	�`B	�fB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
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
+B
1B
1B
	7B
	7B
	7B

=B

=B

=B

=B
JB
PB
VB
\B
bB
hB
uB
uB
uB
{B
�B
�B
�B
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
!�B
"�B
"�B
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
)�B
+B
+B
+B
+B
+B
+B
,B
,B
,B
,B
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
33B
33B
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
6FB
7LB
7LB
8RB
8RB
8RB
8RB
8RB
9XB
9XB
9XB
9XB
:^B
;dB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
=qB
>wB
>wB
>wB
?}B
?}B
?}B
?}B
?}B
@�B
@�B
@�B
@�B
@�B
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
E�B
F�B
G�B
G�B
H�B
I�B
I�B
I�B
I�B
J�B
K�B
K�B
K�B
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
P�B
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
S�B
S�B
T�B
T�B
T�B
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
\)B
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
_;B
_;B
_;B
`BB
`BB
`BB
aHB
`BB
aHB
aHB
aHB
bNB
bNB
cTB
dZB
dZB
dZB
dZB
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
ffB
ffB
gmB
gmB
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
n�B
n�B
o�B
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
s�B
s�B
s�B
t�B
t�B
t�B
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
x�B
x�B
x�B
y�B
y�B
y�B
z�B
z�B
z�B
z�B
{�B
{�B
{�B
{�B
{�B
{�B
|�B
|�B
|�B
|�B
|�B
|�B
}�B
}�B
}�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BQ�BQ�BQ�BQ�BQ�BQ�BQ�BQ�BQ�BQ�BQ�BQBQNBR:BRoBt�B�KB��B��B�TB�nB�nB��B��B�B��B�rB�rB��B��B�	B�RB�B��B�B�|B�kB�.B��B�~BB{Bz�B{dB}<B{dBw�Bn}Ba�Be`BhXB^�BG�B0oB,�B%`B=B�B�BB�.B�B�B��B��B�B�:B�#B��B�*B��B��B�|B��B�4B��BxRBm]BgRBbhB\CBUMBM�BHfB?}B1�B*0BQB�B�B B�BzB
��B
��B
�B
��B
�B
��B
�B
�OB
�B
��B
�tB
{�B
w�B
t�B
p�B
l�B
_!B
U2B
G�B
;JB
/OB
(XB
#TB
$B
B	��B	�B	�B	�BB	��B	ɺB	��B	��B	��B	��B	�GB	�}B	�>B	��B	��B	�MB	�TB	�BB	��B	��B	��B	�3B	��B	yXB	rB	m�B	c�B	^jB	[�B	[�B	[�B	[�B	[WB	Z�B	U�B	O�B	LB	G�B	BuB	@4B	=B	:�B	:xB	6+B	4B	1'B	.�B	'B	!HB	B	�B	�B	�B		B	7B	EB	MB	hB	(B	
	B��B�B�UB�]B�B�B��BیB�+B�FB�vB͟B��BɺB��B�mB��B�B�;B�.B��B�B�0B�XB�B��B�B��B��B��B��B��B��B�4B� B��B�+B�?B��B�;B|�Bu�Bs�BrGBrBqABp�Bn}BjBe`Bc BabBa-B]�BZQBW�BU�BS�BR�BQNBNVBIBF�BD�BCGBB[BAUB@4B?B>(B>]B<�B;�B;�B;JB:�B:B72B5tB2�B2�B1�B1�B1�B/OB-�B-�B-]B,B+6B*�B(�B(>B&�B&�B&2B%`B# B!�B!�B"B!-B!-B!|B �B!B �B �B �B!-B!HB!|B �B �B!�B# B# B#:B$&B$&B%,B$ZB$�B%B&�B)B(�B'�B'�B)_B*KB*�B+�B,�B.�B1B3�B4nB4�B4�B5�B6�B7�B8B9�B:�B;B<PB>BB>�BDMBD3BF%BF�BJ�BQNBQ�BS�BV�BX�BZQBZ�B[�Bb4BeBh$BjBlWBn/Bq[BwBxBxRBz*B|6B}VB~(BcB�iB�uB�SB��B��B��B��B��B��B��B��B�.B��B��B��B�B�IB�-B�nB�fB�=B�wB��B��B��B��B��B��B��B��B��B�'B��B�+B�B�6B�.B�.B�&B�FB�2B�9B�sBچB��B�B�B�zB�B�B��B��B�B�GB�2B��B�B�B�6B�]B	 �B	�B		�B	�B	�B	�B	�B	�B	B	�B	 B	#B	%B	&B	)*B	-)B	/5B	0UB	1AB	2GB	2GB	2aB	2GB	3hB	5tB	9�B	=�B	?�B	?�B	?�B	@�B	D�B	E�B	I�B	K�B	NB	Q B	RB	TB	TB	UB	UB	UB	VB	VB	W$B	W$B	W$B	X+B	X_B	XEB	Z�B	^jB	`\B	abB	b�B	f�B	i�B	i�B	j�B	l�B	n�B	p�B	tB	u�B	u�B	u�B	u�B	y$B	z�B	|B	}B	}"B	}<B	HB	�;B	�'B	�GB	�MB	�YB	�_B	�fB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	�B	�,B	�$B	�$B	�*B	�0B	�6B	�"B	�=B	�CB	�CB	�OB	�OB	�[B	�aB	�GB	�MB	�nB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ªB	��B	��B	ĶB	żB	żB	��B	��B	�	B	�B	�B	�(B	� B	�B	�B	�B	�B	�9B	�?B	�$B	�$B	�EB	�EB	�EB	�B	�qB	ܒB	�jB	ߊB	�vB	�bB	�|B	�B	�tB	�tB	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�	B	��B	��B	�B	��B	�B	�6B	�B	�B	�B	�B	�B	�.B
;B
oB
3B
MB
9B
9B
%B
?B
YB
+B
EB
+B
EB
_B
_B
1B
KB
	RB
	lB
	RB

=B

rB

XB

rB
~B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 B
 �B
!�B
"�B
"�B
$B
$B
#�B
$�B
$�B
%�B
%�B
&�B
'B
($B
($B
(
B
(
B
)*B
*0B
+B
+6B
+6B
+6B
+B
+6B
,"B
,=B
,"B
,WB
.B
.IB
.IB
.IB
./B
/5B
/5B
/5B
/iB
0;B
0UB
0;B
1AB
1AB
1[B
2aB
3MB
3hB
4nB
4TB
5tB
5ZB
5?B
5ZB
6FB
6FB
6`B
6`B
6zB
7�B
7fB
8lB
8lB
8lB
8RB
8�B
9rB
9�B
9rB
9�B
:�B
;�B
<jB
<jB
<jB
<�B
<�B
<�B
<�B
=�B
>�B
>�B
>�B
?�B
?�B
?�B
?�B
?�B
@�B
@�B
@�B
@�B
@�B
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
E�B
F�B
G�B
G�B
H�B
I�B
I�B
I�B
I�B
J�B
K�B
K�B
K�B
L�B
MB
L�B
M�B
M�B
M�B
N�B
OB
OB
N�B
N�B
PB
O�B
O�B
Q B
QB
Q B
RB
RB
RB
RB
SB
SB
SB
S�B
TB
TB
UB
UB
U2B
V9B
VB
VB
W?B
W$B
XEB
XEB
X+B
Y1B
YB
YB
YKB
Y1B
Y1B
Y1B
Y1B
Z7B
ZQB
Z7B
Z7B
[=B
[=B
[WB
\)B
\CB
\CB
\CB
]/B
]/B
]dB
]dB
]IB
^OB
^OB
^jB
_VB
_VB
_VB
`\B
`vB
`\B
aHB
`\B
aHB
a|B
a|B
b�B
b�B
c�B
dtB
d�B
dtB
d�B
ezB
e`B
ezB
e`B
e`B
e`B
e`B
ezB
ezB
e`B
e`B
ezB
ezB
ezB
ezB
ezB
f�B
f�B
g�B
g�B
h�B
h�B
h�B
h�B
hsB
h�B
i�B
i�B
iyB
iyB
i�B
i�B
jB
j�B
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
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
o�B
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
s�B
s�B
s�B
t�B
t�B
t�B
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
xB
xB
x�B
x�B
x�B
y�B
zB
y�B
{B
z�B
z�B
{B
{�B
{�B
|B
{�B
|B
|B
}B
}"B
}B
}B
}B
}"B
~(B
~B
~1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.05(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         202004080035582020040800355820200408003558202211182142342022111821423420221118214234202004090017102020040900171020200409001710  JA  ARFMdecpA19c                                                                20200329003805  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20200328153812  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20200328153814  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20200328153815  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20200328153816  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20200328153816  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20200328153816  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcp2.8e                                                                20200328153816  QCF$                G�O�G�O�G�O�            8000JA  ARGQaqcp2.8e                                                                20200328153816  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20200328153816  QCF$                G�O�G�O�G�O�            8000JA  ARGQrqcpt16c                                                                20200328153818  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20200328153818                      G�O�G�O�G�O�                JA  ARUP                                                                        20200328155439                      G�O�G�O�G�O�                JM  ARGQrqcjv291                                                                20200328153404  QCP$                G�O�G�O�G�O�2DEB7C          JM  ARGQJMQC2.0                                                                 20200328153339  CV  JULD            G�O�G�O�F�l�                JM  ARCAJMQC2.0                                                                 20200407153558  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20200407153558  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20200408151710  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20221118124234  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20221123114511                      G�O�G�O�G�O�                