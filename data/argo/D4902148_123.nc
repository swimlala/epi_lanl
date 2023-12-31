CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       ~2017-12-29T18:35:56Z creation;2017-12-29T18:35:59Z conversion to V3.1;2019-12-18T07:25:55Z update;2022-11-21T05:31:28Z update;     
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
_FillValue                 �  ]$   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  a   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t|   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �d   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �h   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ݈   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20171229183556  20221123111507  4902148 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               {A   JA  I1_0397_123                     2C  Dd�NAVIS_A                         0397                            ARGO 011514                     863 @�@��� 1   @�@�www�@;g�O�;d�d�E��1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�33A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@���@�ff@�ffA33A?33A_33A33A���A���A���A���Aϙ�Aߙ�AA���B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fC�3C�3C�3C�3C	�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C!�3C#�3C%�3C'�3C)�3C+�3C-�3C/�3C1�3C3�3C5�3C7�3C9�3C;�3C=�3C?�3CA�3CC�3CE�3CG�3CI�3CK�3CM�3CO�3CQ�3CS�3CU�3CW�3CY�3C[�3C]�3C_�3Ca�3Cc�3Ce�3Cg�3Ci�3Ck�3Cm�3Co�3Cq�3Cs�3Cu�3Cw�3Cy�3C{�3C}�3C�3C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�fC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D |�D ��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D	|�D	��D
|�D
��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D |�D ��D!|�D!��D"|�D"��D#|�D#��D$|�D$��D%|�D%��D&|�D&��D'|�D'��D(|�D(��D)|�D)��D*|�D*��D+|�D+��D,|�D,��D-|�D-��D.|�D.��D/|�D/��D0|�D0��D1|�D1��D2|�D2��D3|�D3��D4|�D4��D5|�D5��D6|�D6��D7|�D7��D8|�D8��D9|�D9��D:|�D:��D;|�D;��D<|�D<��D=|�D=��D>|�D>��D?|�D?��D@|�D@��DA|�DA��DB|�DB��DC|�DC��DD|�DD��DE|�DE��DF|�DF��DG|�DG��DH|�DH��DI|�DI��DJ|�DJ��DK|�DK��DL|�DL��DM|�DM��DN|�DN��DO|�DO��DP|�DP��DQ|�DQ��DR|�DR��DS|�DS��DT|�DT��DU|�DU��DV|�DV��DW|�DW��DX|�DX��DY|�DY��DZ|�DZ��D[|�D[��D\|�D\��D]|�D]��D^|�D^��D_|�D_��D`|�D`��Da|�Da��Db|�Db��Dc|�Dc��Dd|�Dd��De|�De��Df|�Df��Dg|�Dg��Dh|�Dh��Di|�Di��Dj|�Dj��Dk|�Dk��Dl|�Dl��Dm|�Dm��Dn|�Dn��Do|�Do��Dp|�Dp��Dq|�Dq��Dr|�Dr��Ds|�Ds��Dt|�Dt��Du|�Du��Dv|�Dv��Dw|�Dw��Dx|�Dx��Dy|�Dy��Dz|�Dz��D{|�D{��D||�D|��D}|�D}��D~|�D~��D|�D��D�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��D�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD¾fD��fD�>fD�~fDþfD��fD�>fD�~fDľfD��fD�>fD�~fDžfD��fD�>fD�~fDƾfD��fD�>fD�~fDǾfD��fD�>fD�~fDȾfD��fD�>fD�~fDɾfD��fD�>fD�~fDʾfD��fD�>fD�~fD˾fD��fD�>fD�~fD̾fD��fD�>fD�~fD;fD��fD�>fD�~fDξfD��fD�>fD�~fDϾfD��fD�>fD�~fDоfD��fD�>fD�~fDѾfD��fD�>fD�~fDҾfD��fD�>fD�~fDӾfD��fD�>fD�~fDԾfD��fD�>fD�~fDվfD��fD�>fD�~fD־fD��fD�>fD�~fD׾fD��fD�>fD�~fDؾfD��fD�>fD�~fDپfD��fD�>fD�~fDھfD��fD�>fD�~fD۾fD��fD�>fD�~fDܾfD��fD�>fD�~fDݾfD��fD�>fD�~fD޾fD��fD�>fD�~fD߾fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD��fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��\A���A���A���A���A���A���A���A���A���A���A���A���A��\A��PA��\A��hA���A��uA��uA��uA���A���A��uA��uA���A���A���A���A���A���A���A���A���A���A���A���A���A��PA��DA�r�A�C�A�ƨA�=qA�ffA�I�A��+A�/A�z�A��A��A���A���A�{A��A��;A���A��#A�|�A�=qA�jA��hA���A�ZA�~�A�VA�A�XA��A�%A��A�JA~VA~I�A~E�A~JA}�TA}��A}�A|�!A{"�Ay/Ax  AwhsAw+AvĜAv��AvM�Au��Au�hAt�`As�Apv�Am�AlQ�Ak��AkK�Aj��Ai|�Ahr�Ag�AgAf��Af�Af �Aex�Ad9XAbz�Aa
=A_K�A\�RA\bNA\�!A\��A\�RA\��A[�#AY��AW\)AV  AT�AS�mASG�ARjAP��ANM�AM��AK�wAI�;AH�AG�AF�AFE�AE�AD9XAB��ABv�ABbAAO�A@�!A@ZA?��A?\)A=�^A=t�A=��A=��A=��A=�A=��A=�A=�7A=t�A=;dA<�uA;��A;C�A9�;A9O�A8��A7�hA6ffA5/A4�\A4��A4�DA4(�A3��A3�FA2�yA0ZA.��A,ĜA*=qA)33A(�A'�mA'��A'&�A&M�A%�A%�hA%\)A$1A"�9A"A ��A =qA��Al�A�!A�Ax�A�A{A�AVA
=An�A(�Az�AƨA?}A�RA(�A�-A?}A�A�Ax�A~�A�PAS�A�A  A%A
�A	�PA��AC�A`BA�AĜA�jA��A�A��A �9@�\)@���@�/@��@�Ĝ@��D@�1@��
@�K�@���@��@��7@�`B@�I�@�dZ@��-@�D@��;@�h@�r�@��y@��`@�=q@�`B@��@���@�x�@��;@�P@��@�Q�@���@�7L@ܬ@���@�t�@�@�K�@ղ-@��@���@�~�@�@�X@�Q�@�t�@�"�@��y@���@Η�@�p�@�r�@�1@ʏ\@ɲ-@ɩ�@�V@�ȴ@��@ź^@�x�@�/@�Z@�\)@\@�n�@�E�@���@��@�7L@�Ĝ@��F@�M�@�@��7@�p�@�x�@�hs@�`B@�G�@��@�z�@��m@���@��-@�r�@�M�@���@�bN@�1'@��m@���@�t�@�S�@�
=@�V@�G�@��
@��@���@��m@�5?@��@���@���@��h@�p�@�&�@��`@��@��@���@��#@�?}@��j@�  @��H@��\@���@���@��F@��R@�@��@�&�@�V@�Z@���@�t�@�
=@��R@�V@�$�@�J@���@��@��#@���@���@�  @���@�;d@���@���@�ff@�5?@���@���@�O�@���@�r�@��@�V@��@���@��7@�X@�V@�r�@���@�;d@�o@���@���@�n�@�5?@�/@�1'@�1'@�1@��
@���@�ƨ@�t�@�33@�@��!@�n�@�-@��@��@��j@��m@��P@�|�@�t�@�S�@�C�@�33@�+@�"�@�"�@�@���@�n�@�M�@���@��@��T@���@��h@�G�@�/@�V@��`@���@�Q�@��@��!@���@�^5@�E�@��@�1'@~�R@~$�@}@}�-@}�-@}�@}/@|��@z�@z=q@y�@y�7@x�9@x �@w�@wK�@w�@v��@v�R@w
=@vȴ@vff@u��@u�-@u��@u�@t��@tj@tj@tz�@tZ@tI�@t(�@s��@r�H@q��@qx�@pb@o�w@o�@o�@o�@o�P@n��@nff@m�T@m��@mp�@m/@mV@l�/@lZ@l�@k��@kƨ@k�@kt�@kdZ@kS�@kS�@kS�@k"�@k@j�@j��@j��@j��@j~�@j=q@j=q@j=q@j-@jJ@i�#@i�^@i�7@iG�@hĜ@h�@hA�@g;d@fv�@f$�@e@e�@eO�@d�@d�D@c�
@a�@aG�@a%@`��@_��@_�@^�@^ȴ@^ȴ@^��@^��@^��@^v�@^$�@]��@]�@]/@]V@\Z@[�@Z�@Y��@Y%@XĜ@X�u@XQ�@X1'@Xb@W�@W�@V�y@V��@V�+@Vff@V5?@U�@U�T@U��@U�h@U`B@U�@T��@T�@T9X@S�F@St�@R��@Q��@Qx�@P��@P1'@O�P@OK�@O�@N��@N�@N��@NE�@M��@M@M��@M�h@M�@M`B@M/@M�@M�@L��@L�D@LI�@K��@K��@K�F@K�F@K��@KdZ@KS�@KC�@Ko@J��@J�!@J^5@J�@I��@I��@I�#@I��@I�7@Ihs@HĜ@HQ�@H �@H �@H �@Hb@Hb@Hb@G�@G�;@G�w@G�P@F�+@E�@D��@D�D@D1@C�F@C��@CS�@B�H@B��@B~�@BM�@A�@A��@AG�@@�9@@�@?;d@>@=O�@=/@=V@<��@<��@<�@<�/@<�j@<�@<z�@<I�@<1@;��@;�
@;��@;��@;��@;��@;��@;��@;��@;��@;S�@;C�@;o@:��@:�\@:n�@9�@9%@8Ĝ@8�9@8Q�@8  @7�@7�@7�@7�@7�;@7��@7��@6�@6v�@6ff@6E�@6{@5�-@5�@5?}@5?}@5?}@5�@4�j@4z�@4Z@49X@3��@3�
@3�F@3�@3t�@3S�@3"�@2��@2~�@2n�@2^5@2M�@2M�@2-@2J@1��@1hs@1�@1�@1�@0�`@0 �@/�@.�R@.�+@.$�@-�h@-V@,��@,�@,9X@,1@,1@,�@,1@+��@+�m@+�m@+��@+��@+ƨ@+�F@+�F@+�F@+��@+33@*M�@)�^@)�@(Ĝ@(�u@(bN@(A�@'�@'�w@'��@'|�@'l�@'\)@';d@&ȴ@%�@%�@%�@$�@$I�@$(�@$(�@#t�@#t�@#dZ@#t�@#t�@#dZ@#dZ@#dZ@#S�@#C�@#33@#33@#33@#33@#33@#C�@#33@#"�@"�@"��@"�\@!x�@ r�@ Q�@ Q�@ Q�@ b@�;@��@��@�w@�w@\)@V@��@9X@1@1@1@1@ƨ@�F@��@��@��@t�@@��@�!@~�@^5@�@�@��@G�@%@�u@r�@Q�@�@��@�w@;d@
=@��@��@�y@�y@��@ff@$�@@��@�h@`B@O�@O�@O�@?}@/@�/@j@I�@�@��@ƨ@t�@�\@�#@�^@��@X@7L@�@�`@��@�u@�@l�@�@�R@��@��@�+@v�@v�@v�@v�@V@{@��@�-@�h@p�@O�@V@�@Z@��@
��@
�\@
^5@	��@	�^@	��@	��@	��@	�7@	�@��@�@Q�@ �@  @��@��@K�@��@��@E�@@�T@�T@��@�T@��@��@@p�@�@�D@j@Z@Z@Z@I�@I�@I�@I�@9X@9X@9X@9X@9X@�@1@�F@��@��@��@�@�@�@�@S�@"�@@�@�H@��@��@�\@�\@~�@~�@^5@=q@-@-@��@�#@��@��@��@��@��@��@�@ ��@ ��@ ��@ ��@ ��@ �u@ �u@ r�@  �@   ?�|�?��?��R?���?�V?�V?�5??��?��-?�p�?�V?���?��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��\A���A���A���A���A���A���A���A���A���A���A���A���A��\A��PA��\A��hA���A��uA��uA��uA���A���A��uA��uA���A���A���A���A���A���A���A���A���A���A���A���A���A��PA��DA�r�A�C�A�ƨA�=qA�ffA�I�A��+A�/A�z�A��A��A���A���A�{A��A��;A���A��#A�|�A�=qA�jA��hA���A�ZA�~�A�VA�A�XA��A�%A��A�JA~VA~I�A~E�A~JA}�TA}��A}�A|�!A{"�Ay/Ax  AwhsAw+AvĜAv��AvM�Au��Au�hAt�`As�Apv�Am�AlQ�Ak��AkK�Aj��Ai|�Ahr�Ag�AgAf��Af�Af �Aex�Ad9XAbz�Aa
=A_K�A\�RA\bNA\�!A\��A\�RA\��A[�#AY��AW\)AV  AT�AS�mASG�ARjAP��ANM�AM��AK�wAI�;AH�AG�AF�AFE�AE�AD9XAB��ABv�ABbAAO�A@�!A@ZA?��A?\)A=�^A=t�A=��A=��A=��A=�A=��A=�A=�7A=t�A=;dA<�uA;��A;C�A9�;A9O�A8��A7�hA6ffA5/A4�\A4��A4�DA4(�A3��A3�FA2�yA0ZA.��A,ĜA*=qA)33A(�A'�mA'��A'&�A&M�A%�A%�hA%\)A$1A"�9A"A ��A =qA��Al�A�!A�Ax�A�A{A�AVA
=An�A(�Az�AƨA?}A�RA(�A�-A?}A�A�Ax�A~�A�PAS�A�A  A%A
�A	�PA��AC�A`BA�AĜA�jA��A�A��A �9@�\)@���@�/@��@�Ĝ@��D@�1@��
@�K�@���@��@��7@�`B@�I�@�dZ@��-@�D@��;@�h@�r�@��y@��`@�=q@�`B@��@���@�x�@��;@�P@��@�Q�@���@�7L@ܬ@���@�t�@�@�K�@ղ-@��@���@�~�@�@�X@�Q�@�t�@�"�@��y@���@Η�@�p�@�r�@�1@ʏ\@ɲ-@ɩ�@�V@�ȴ@��@ź^@�x�@�/@�Z@�\)@\@�n�@�E�@���@��@�7L@�Ĝ@��F@�M�@�@��7@�p�@�x�@�hs@�`B@�G�@��@�z�@��m@���@��-@�r�@�M�@���@�bN@�1'@��m@���@�t�@�S�@�
=@�V@�G�@��
@��@���@��m@�5?@��@���@���@��h@�p�@�&�@��`@��@��@���@��#@�?}@��j@�  @��H@��\@���@���@��F@��R@�@��@�&�@�V@�Z@���@�t�@�
=@��R@�V@�$�@�J@���@��@��#@���@���@�  @���@�;d@���@���@�ff@�5?@���@���@�O�@���@�r�@��@�V@��@���@��7@�X@�V@�r�@���@�;d@�o@���@���@�n�@�5?@�/@�1'@�1'@�1@��
@���@�ƨ@�t�@�33@�@��!@�n�@�-@��@��@��j@��m@��P@�|�@�t�@�S�@�C�@�33@�+@�"�@�"�@�@���@�n�@�M�@���@��@��T@���@��h@�G�@�/@�V@��`@���@�Q�@��@��!@���@�^5@�E�@��@�1'@~�R@~$�@}@}�-@}�-@}�@}/@|��@z�@z=q@y�@y�7@x�9@x �@w�@wK�@w�@v��@v�R@w
=@vȴ@vff@u��@u�-@u��@u�@t��@tj@tj@tz�@tZ@tI�@t(�@s��@r�H@q��@qx�@pb@o�w@o�@o�@o�@o�P@n��@nff@m�T@m��@mp�@m/@mV@l�/@lZ@l�@k��@kƨ@k�@kt�@kdZ@kS�@kS�@kS�@k"�@k@j�@j��@j��@j��@j~�@j=q@j=q@j=q@j-@jJ@i�#@i�^@i�7@iG�@hĜ@h�@hA�@g;d@fv�@f$�@e@e�@eO�@d�@d�D@c�
@a�@aG�@a%@`��@_��@_�@^�@^ȴ@^ȴ@^��@^��@^��@^v�@^$�@]��@]�@]/@]V@\Z@[�@Z�@Y��@Y%@XĜ@X�u@XQ�@X1'@Xb@W�@W�@V�y@V��@V�+@Vff@V5?@U�@U�T@U��@U�h@U`B@U�@T��@T�@T9X@S�F@St�@R��@Q��@Qx�@P��@P1'@O�P@OK�@O�@N��@N�@N��@NE�@M��@M@M��@M�h@M�@M`B@M/@M�@M�@L��@L�D@LI�@K��@K��@K�F@K�F@K��@KdZ@KS�@KC�@Ko@J��@J�!@J^5@J�@I��@I��@I�#@I��@I�7@Ihs@HĜ@HQ�@H �@H �@H �@Hb@Hb@Hb@G�@G�;@G�w@G�P@F�+@E�@D��@D�D@D1@C�F@C��@CS�@B�H@B��@B~�@BM�@A�@A��@AG�@@�9@@�@?;d@>@=O�@=/@=V@<��@<��@<�@<�/@<�j@<�@<z�@<I�@<1@;��@;�
@;��@;��@;��@;��@;��@;��@;��@;��@;S�@;C�@;o@:��@:�\@:n�@9�@9%@8Ĝ@8�9@8Q�@8  @7�@7�@7�@7�@7�;@7��@7��@6�@6v�@6ff@6E�@6{@5�-@5�@5?}@5?}@5?}@5�@4�j@4z�@4Z@49X@3��@3�
@3�F@3�@3t�@3S�@3"�@2��@2~�@2n�@2^5@2M�@2M�@2-@2J@1��@1hs@1�@1�@1�@0�`@0 �@/�@.�R@.�+@.$�@-�h@-V@,��@,�@,9X@,1@,1@,�@,1@+��@+�m@+�m@+��@+��@+ƨ@+�F@+�F@+�F@+��@+33@*M�@)�^@)�@(Ĝ@(�u@(bN@(A�@'�@'�w@'��@'|�@'l�@'\)@';d@&ȴ@%�@%�@%�@$�@$I�@$(�@$(�@#t�@#t�@#dZ@#t�@#t�@#dZ@#dZ@#dZ@#S�@#C�@#33@#33@#33@#33@#33@#C�@#33@#"�@"�@"��@"�\@!x�@ r�@ Q�@ Q�@ Q�@ b@�;@��@��@�w@�w@\)@V@��@9X@1@1@1@1@ƨ@�F@��@��@��@t�@@��@�!@~�@^5@�@�@��@G�@%@�u@r�@Q�@�@��@�w@;d@
=@��@��@�y@�y@��@ff@$�@@��@�h@`B@O�@O�@O�@?}@/@�/@j@I�@�@��@ƨ@t�@�\@�#@�^@��@X@7L@�@�`@��@�u@�@l�@�@�R@��@��@�+@v�@v�@v�@v�@V@{@��@�-@�h@p�@O�@V@�@Z@��@
��@
�\@
^5@	��@	�^@	��@	��@	��@	�7@	�@��@�@Q�@ �@  @��@��@K�@��@��@E�@@�T@�T@��@�T@��@��@@p�@�@�D@j@Z@Z@Z@I�@I�@I�@I�@9X@9X@9X@9X@9X@�@1@�F@��@��@��@�@�@�@�@S�@"�@@�@�H@��@��@�\@�\@~�@~�@^5@=q@-@-@��@�#@��@��@��@��@��@��@�@ ��@ ��@ ��@ ��@ ��@ �u@ �u@ r�@  �@   ?�|�?��?��R?���?�V?�V?�5??��?��-?�p�?�V?���?��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�BuBoBVBB�!BgmBA�B �BB�mB�9B��B�BhsBS�BI�B<jB.B�BuBDB
�B
��B
ÖB
�?B
�B
��B
��B
�uB
�+B
w�B
\)B
F�B
8RB
1'B
Q�B
iyB
jB
k�B
jB
jB
o�B
k�B
ffB
bNB
_;B
^5B
\)B
[#B
YB
VB
R�B
M�B
D�B
-B
�B
DB
B
B	��B	�B	�yB	�TB	�;B	�;B	�BB	�/B	�B	��B	�jB	�B	��B	�=B	�oB	��B	��B	��B	��B	��B	�uB	� B	u�B	gmB	ZB	Q�B	H�B	<jB	+B	)�B	�B	DB��B��B	%B	�B	�B	hB	
=B	1B	%B	B	B	B��B��B�B��B��B	  B	B	+B	+B	%B	%B	B	%B	B��B��B��B�B�B�yB�HB�B�B�/B�/B�)B�#B�B�B��BŢB�qB�RB�?B�-B�!B�B�B��B��B��B��B��B��B��B��B�uB�oB�bB�VB�DB�=B�+B�B� By�Bu�Br�Bp�Bm�Bk�BiyBhsBffBe`BcTBbNB`BB^5B\)BYBXBVBS�BQ�BO�BM�BK�BH�BE�BD�BD�BC�BB�B?}B<jB:^B9XB8RB8RB8RB8RB8RB8RB7LB7LB6FB6FB5?B49B49B33B33B2-B0!B0!B/B.B-B-B,B+B+B)�B,B,B)�B+B,B,B,B+B)�B(�B)�B+B,B-B-B-B.B/B0!B0!B0!B0!B/B0!B1'B1'B2-B49B33B33B6FB7LB7LB8RB8RB9XB;dB<jB<jB<jB=qB>wB=qB=qB?}BB�BB�BC�BC�BC�BC�BC�BC�BC�BC�BD�BE�BF�BG�BI�BL�BM�BM�BN�BO�BO�BO�BP�BQ�BS�BW
BYB\)BaHBffBgmBgmBgmBhsBhsBhsBiyBiyBjBn�Bq�Bs�Bt�Bv�By�Bz�B|�B�B�B�1B�DB�JB�VB�VB�hB�{B�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�!B�-B�9B�jB��B��BBĜBŢBƨBɺB��B��B��B��B��B�B�
B�5B�`B�`B�fB�sB�sB�sB�B�B�B�B�B��B��B��B��B	B	+B	1B	1B		7B		7B	
=B	
=B	
=B	
=B	
=B	PB	VB	\B	hB	oB	oB	uB	{B	�B	�B	�B	�B	�B	�B	#�B	/B	0!B	2-B	5?B	8RB	7LB	=qB	@�B	C�B	C�B	C�B	C�B	D�B	E�B	K�B	M�B	N�B	O�B	T�B	W
B	ZB	ZB	[#B	]/B	bNB	gmB	iyB	jB	m�B	m�B	m�B	m�B	m�B	n�B	q�B	s�B	s�B	s�B	t�B	u�B	u�B	u�B	w�B	|�B	� B	� B	� B	� B	� B	�B	�B	�B	�+B	�1B	�=B	�=B	�DB	�\B	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�'B	�3B	�9B	�?B	�FB	�LB	�XB	�^B	�jB	B	ÖB	ĜB	ÖB	ŢB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�
B	�B	�B	�B	�B	�#B	�#B	�#B	�#B	�/B	�5B	�5B	�5B	�;B	�BB	�BB	�BB	�HB	�HB	�NB	�NB	�NB	�ZB	�`B	�`B	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
1B
	7B

=B

=B
DB
JB
JB
PB
PB
VB
VB
\B
bB
bB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
#�B
$�B
$�B
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
'�B
(�B
(�B
(�B
(�B
)�B
)�B
)�B
)�B
+B
+B
,B
,B
,B
,B
,B
,B
,B
-B
.B
.B
.B
.B
/B
0!B
1'B
1'B
1'B
2-B
33B
49B
49B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
6FB
8RB
9XB
:^B
:^B
;dB
;dB
;dB
;dB
<jB
<jB
<jB
<jB
<jB
<jB
=qB
>wB
>wB
?}B
@�B
A�B
A�B
A�B
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
F�B
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
M�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
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
Q�B
Q�B
R�B
R�B
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
ZB
ZB
ZB
[#B
[#B
[#B
[#B
\)B
^5B
^5B
^5B
_;B
_;B
_;B
_;B
_;B
_;B
`BB
aHB
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
dZB
e`B
e`B
ffB
gmB
hsB
hsB
hsB
iyB
iyB
iyB
iyB
iyB
jB
jB
k�B
k�B
k�B
k�B
l�B
l�B
l�B
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
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
r�B
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
s�B
s�B
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
u�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
x�B
x�B
x�B
x�B
x�B
x�B
y�B
y�B
y�B
y�B
z�B
z�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111BsB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B[B BB�fBlWBFYB$�BB��B�8B�WB�_Bl=BVBL0B?HB0�B!�BBpB
�fB
�B
�SB
��B
��B
��B
�9B
�2B
��B
{B
^�B
H�B
9�B
1�B
RB
i�B
j�B
lB
k6B
k�B
qvB
m�B
g�B
b�B
_�B
^�B
\xB
[�B
Y�B
V�B
T,B
O�B
H�B
0!B

B
B
�B
�B	�qB	��B	�B	��B	ߤB	ߤB	�-B	�OB	��B	��B	�wB	�cB	�!B	��B	�TB	��B	��B	�FB	��B	�bB	�B	��B	w2B	h�B	[=B	SuB	KB	>�B	,�B	,WB	B	6B	 B�.B	�B	�B	1B	�B	
�B		B	+B	B	�B	�B��B�jB�B��B��B��B	9B	_B	zB	tB	tB	�B	B	'B��B�jB��B��B�B�B�B��B�1B�dBݲB��B��B��B�B�(B�fB�iB��B�zB��B��B��B�B��B��B��B��B��B��B��B��B�FB��B��B�vB��B�^B��B�B��B|Bv�Bs�Br�Bn�BlWBjKBiDBgBfBdBcnBabB_�B]IBY�BYKBW
BUMBS@BP�BOBBM�BJ�BF?BD�BD�BDMBD�BA�B=�B;�B:^B8�B8�B8�B8�B8�B8�B7�B7�B6�B6�B5�B5B5B4TB4B2�B1�B1'B0oB/�B.�B-�B,�B,WB,"B+B,�B,�B+�B,�B,�B,�B,�B+�B+QB*�B+QB,=B,�B-wB-�B-�B.�B/�B0oB0oB0oB0�B0B0�B1�B2-B2�B4�B4B4�B6�B7�B7�B8�B9	B:*B;�B<�B<�B<�B=�B>�B=�B>]B@�BB�BB�BC�BC�BC�BC�BC�BC�BDBD3BE�BFtBG�BIBJ�BMBNBN"BO(BPBPBPbBQ�BR�BUBW�BZ7B]dBbNBf�Bg�Bg�Bg�Bh�Bh�Bh�Bi�BjBk�BoOBr-Bt9BuZBw�BzDB{B}�B��B��B��B��B��B��B�B�B��B��B��B��B��B��B��B��B��B��B�CB�\B�&B�2B�8B�*B�KB�6B�WB�wB�iB��B��B�%B��B��B��B��B��B��B�EB�XB�(B�B� B�FB�MB�mB��B��B�`B�B�B�B�B��B��B��B� B��B��B�B�LB��B�}B	SB	EB	fB	KB		RB		RB	
XB	
rB	
XB	
rB	
�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	B	B	OB	$tB	/OB	0oB	2|B	5�B	9>B	7�B	=�B	@�B	C�B	C�B	C�B	C�B	EB	F?B	K�B	N"B	O(B	PHB	U2B	WYB	Z7B	ZQB	[WB	]/B	bNB	g�B	i�B	j�B	m�B	m�B	m�B	m�B	m�B	n�B	q�B	s�B	s�B	s�B	uB	vB	vFB	u�B	x8B	}B	� B	�B	�B	�4B	�iB	�GB	�GB	�9B	�EB	�fB	�XB	�XB	�xB	�vB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�
B	�*B	�B	�KB	�6B	�=B	�qB	�vB	�hB	��B	�tB	�zB	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�HB	�TB	�uB	�YB	�YB	�1B	�1B	�QB	�7B	�=B	�=B	�WB	یB	�IB	�OB	�OB	�OB	�VB	�BB	�\B	�vB	�bB	�bB	�hB	�B	�B	�B	�B	�B	�B	��B	�B	��B	��B	�B	�B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	�B	��B	��B	�	B	��B	�B	�B	��B	�B	��B	�B	�B	�"B	�B	�BB	�HB
 B
B
B
B
B
B
 B
;B
 B
;B
oB
gB
YB
zB
fB
	lB

rB

XB
xB
dB
~B
jB
�B
pB
�B
�B
�B
 B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
 �B
 �B
 �B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
"B
#�B
$�B
%B
%B
%,B
&B
&B
&�B
'B
'B
'B
(
B
(
B
(
B
)B
)*B
)B
)B
*B
*B
*B
*B
+B
+6B
,B
,B
,"B
,"B
,=B
,=B
,=B
-)B
.B
./B
.IB
.}B
/iB
0UB
1AB
1[B
1vB
2|B
3MB
4TB
4�B
5ZB
5?B
5?B
5?B
5?B
5ZB
5?B
5?B
5?B
5tB
5?B
5?B
5ZB
5ZB
5�B
6�B
8�B
9�B
:xB
:xB
;B
;B
;B
;�B
<�B
<�B
<�B
<�B
<�B
<�B
=�B
>�B
>�B
?�B
@�B
A�B
A�B
A�B
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
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
D�B
EB
F�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
J	B
J=B
KB
KDB
NB
M�B
M�B
M�B
M�B
N�B
N�B
N�B
M�B
OB
OB
PB
PB
O�B
O�B
Q B
QB
Q B
R B
RB
S&B
SB
TB
T,B
U2B
UB
U2B
V9B
VB
VB
W
B
W
B
W$B
W?B
W$B
X+B
X+B
X+B
YKB
Y1B
YB
YB
YB
Y1B
Y1B
Z7B
Z7B
Z7B
[=B
[=B
[WB
[�B
\]B
^OB
^OB
^OB
_pB
_VB
_VB
_VB
_pB
_pB
`vB
abB
b�B
b�B
bNB
bNB
bNB
cTB
cnB
cnB
cnB
c�B
cnB
cnB
dtB
dtB
d�B
dtB
d�B
e�B
e�B
f�B
g�B
h�B
h�B
h�B
iyB
iyB
i�B
i�B
i�B
j�B
j�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
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
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
r�B
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
s�B
s�B
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
u�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
xB
w�B
xB
y	B
x�B
x�B
x�B
x�B
x�B
y�B
zB
y�B
y�B
z�B
z�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.05(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201801090031512018010900315120180109003151202211182133062022111821330620221118213306201804031938432018040319384320180403193843  JA  ARFMdecpA19c                                                                20171230033514  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20171229183556  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20171229183558  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20171229183558  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20171229183559  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20171229183559  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20171229183559  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20171229183559  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20171229183559  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20171229183559                      G�O�G�O�G�O�                JA  ARUP                                                                        20171229185610                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20171230153349  CV  JULD            G�O�G�O�F�                JM  ARCAJMQC2.0                                                                 20180108153151  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180108153151  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180403103843  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200114171536                      G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20221118123306  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20221123111507                      G�O�G�O�G�O�                