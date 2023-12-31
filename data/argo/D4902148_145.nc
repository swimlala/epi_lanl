CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       ~2018-08-06T15:35:12Z creation;2018-08-06T15:35:15Z conversion to V3.1;2019-12-18T07:21:04Z update;2022-11-21T05:30:21Z update;     
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
_FillValue                    �DArgo profile    3.1 1.2 19500101000000  20180806153512  20221123111507  4902148 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I1_0397_145                     2C  Dd3�NAVIS_A                         0397                            ARGO 011514                     863 @�w�8� 1   @�w��O� @<nc�	�d3��	k�1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @   @�  @�  A   A   A@  A`  A�  A�33A�  A�  A�  A�  A���A�  B   B  B  B��B   B(  B/��B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C]�fC`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DYy�DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dك3D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�3D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111f@��@|��@�ff@�ffA33A?33A_33A33A���A���A���A���Aϙ�A�ffAA���B��B��BffB��B'��B/ffB7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fC�3C�3C�3C�3C	�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C!�3C#�3C%�3C'�3C)�3C+�3C-�3C/�3C1�3C3�3C5�3C7�3C9�3C;�3C=�3C?�3CA�3CC�3CE�3CG�3CI�3CK�3CM�3CO�3CQ�3CS�3CU�3CW�3CY�3C[�3C]ٚC_�3Ca�3Cc�3Ce�3Cg�3Ci�3Ck�3Cm�3Co�3Cq�3Cs�3Cu�3Cw�3Cy�3C{�3C}�3C�3C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D |�D ��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D	|�D	��D
|�D
��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D |�D ��D!|�D!��D"|�D"��D#|�D#��D$|�D$��D%|�D%��D&|�D&��D'|�D'��D(|�D(��D)|�D)��D*|�D*��D+|�D+��D,|�D,��D-|�D-��D.|�D.��D/|�D/��D0|�D0��D1|�D1��D2|�D2��D3|�D3��D4|�D4��D5|�D5��D6|�D6��D7|�D7��D8|�D8��D9|�D9��D:|�D:��D;|�D;��D<|�D<��D=|�D=��D>|�D>��D?|�D?��D@|�D@��DA|�DA��DB|�DB��DC|�DC��DD|�DD��DE|�DE��DF|�DF��DG|�DG��DH|�DH��DI|�DI��DJ|�DJ��DK|�DK��DL|�DL��DM|�DM��DN|�DN��DO|�DO��DP|�DP��DQ|�DQ��DR|�DR��DS|�DS��DT|�DT��DU|�DU��DV|�DV��DW|�DW��DX|�DX��DYvfDY��DZ|�DZ��D[|�D[��D\|�D\��D]|�D]��D^|�D^��D_|�D_��D`|�D`��Da|�Da��Db|�Db��Dc|�Dc��Dd|�Dd��De|�De��Df|�Df��Dg|�Dg��Dh|�Dh��Di|�Di��Dj|�Dj��Dk|�Dk��Dl|�Dl��Dm|�Dm��Dn|�Dn��Do|�Do��Dp|�Dp��Dq|�Dq��Dr|�Dr��Ds|�Ds��Dt|�Dt��Du|�Du��Dv|�Dv��Dw|�Dw��Dx|�Dx��Dy|�Dy��Dz|�Dz��D{|�D{��D||�D|��D}|�D}��D~|�D~��D|�D��D�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD���D��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��D�A�D�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD¾fD��fD�>fD�~fDþfD��fD�>fD�~fDľfD��fD�>fD�~fDžfD��fD�>fD�~fDƾfD��fD�>fD�~fDǾfD��fD�>fD�~fDȾfD��fD�>fD�~fDɾfD��fD�>fD�~fDʾfD��fD�>fD�~fD˾fD��fD�>fD�~fD̾fD��fD�>fD�~fD;fD��fD�>fD�~fDξfD��fD�>fD�~fDϾfD��fD�>fD�~fDоfD��fD�>fD�~fDѾfD��fD�>fD�~fDҾfD��fD�>fD�~fDӾfD��fD�>fD�~fDԾfD��fD�>fD�~fDվfD��fD�>fD�~fD־fD��fD�>fD�~fD׾fD��fD�>fD�~fDؾfD��fD�>fDف�DپfD��fD�>fD�~fDھfD��fD�>fD�~fD۾fD��fD�>fD�~fDܾfD��fD�>fD�~fDݾfD��fD�>fD�~fD޾fD��D�>fD�~fD߾fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD��fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111)A�r�A�p�A�l�A�jA�l�A�l�A�l�A�`BA�ZA�Q�A�Q�A�C�A�"�A���A��/A���A�ƨAǰ!A���AA��A���A�jA�  A�%A���A�~�A��A�x�A�9XA��A���A�\)A���A��A���A�1A���A�I�A�&�A��HA�  A��HA�XA���A�|�A�C�A���A�v�A��A��RA�C�A�\)A�jA�+A���A�z�A�$�A��\A�oA��A���A�ffA�v�A�`BA�x�A��\A�1A���A�
=A�ȴA�M�A�E�A�5?A�%A��A�+A}A{;dAw��Au�
At�RAr�/Ap��An�AmdZAl��Alv�Ak��AkoAj�Aj��Ai��Ah�`Ah^5AghsAfAd�jAd  Ac�Ab�DAbjAb-Aa/A_�;A_��A_��A_�A_oA^�`A^5?A\��A\1'A[S�AZn�AYAX^5AWXAU��AT�9AS�mAR��AQ��AQ��AQ?}AO�^AO�AOO�AO&�AOAM��AM%AL�+AL$�AJ�HAJ �AHZAG�TAGt�AFZAEl�AE&�ADffAB��AA�;AA&�A@��A?7LA>VA=��A=oA<��A<M�A;�A;%A9|�A7�A7/A6��A5��A4�A3��A2�jA2$�A1�TA1&�A0v�A0VA0�A/�PA.�HA.ffA-�A-G�A,ȴA,jA+��A+��A*��A*�+A*VA*�A)l�A(VA'��A';dA&ȴA&-A%
=A#�FA#G�A"M�A!\)A ĜA v�Al�A��A�+AA�A�-A?}A�`A�#A�^A�hA|�A`BA?}A��AXA
=AĜA�A��Az�AȴAC�A��A�9AM�A/AĜA��A`BA��AZA�/AbAt�A�A�hA�A
��A
�9A
M�A	"�A�\A�A�A��Av�A\)AA�A�A�A��A �A ^5A $�@�t�@�p�@�(�@���@�;d@��@��#@���@��!@�`B@�t�@�@��`@�@�Z@�1'@��@���@�X@�/@��@��`@�Ĝ@�u@�bN@�1'@�@�^@��@�E�@�-@���@�o@�p�@�j@�S�@�&�@�/@�j@�Z@�S�@�-@թ�@�9X@�V@Η�@��@˕�@�=q@Ɨ�@�G�@Õ�@�/@�j@��@��^@�V@�9X@�K�@��!@�@��@�l�@��y@�=q@�Ĝ@���@�"�@���@�V@�=q@��#@�X@���@�dZ@�=q@���@�
=@��H@��@��@���@���@�ȴ@���@��\@�~�@�ff@�E�@��@�p�@��;@��+@�ff@�V@�-@��-@�X@���@��9@�9X@��P@�ff@��@�bN@�(�@��m@���@�dZ@��R@�M�@��@���@���@�O�@���@�Z@��@���@���@���@�A�@���@��@��P@��@�l�@�S�@�;d@��@�@��@���@�n�@�$�@��@��@��u@�r�@�Q�@�A�@�A�@�A�@�1'@� �@�b@�  @��
@���@�|�@�;d@��y@�$�@���@���@�5?@���@���@�X@��@��@�%@��9@��u@��@�z�@�Q�@�A�@�(�@�l�@��H@�=q@�7L@��
@�33@��R@���@�ff@�=q@�$�@��@��^@���@�x�@�7L@���@�r�@� �@��@��;@��w@���@�t�@�@���@�^5@�=q@�-@�{@�@��#@���@�@��^@��-@���@��@�X@�G�@�7L@�&�@���@�r�@�b@~�R@~v�@~ff@}�@}�h@}p�@}`B@|��@{�m@{C�@{33@{33@{33@{o@z�H@z�!@y��@y�@x�@x1'@w�;@w��@w+@v��@w
=@v��@vff@u�@tj@st�@r~�@q��@qG�@p�@pA�@p1'@p �@p �@p  @o�w@o�@o|�@o\)@o;d@o�@o�@o+@o�@o
=@n��@n�@n�R@n�@n�+@n{@mp�@m�@l�/@lI�@k33@iX@f�@f��@fE�@fv�@fv�@f$�@e`B@d��@d�@c33@a��@`��@`�9@`�@`r�@`r�@`r�@`�@`bN@`b@_��@_|�@^ȴ@]��@]p�@]?}@\�D@\Z@\(�@[��@[�F@[��@[��@[t�@[dZ@["�@Z��@Z�\@Z~�@Z~�@Z~�@Zn�@Z^5@ZM�@Z=q@Z�@Yx�@X�`@XQ�@W\)@W+@W
=@W
=@V�R@V$�@Up�@UV@T��@T1@T1@T1@T1@S��@S�m@S�
@Sƨ@Sƨ@S�F@S�@SS�@S33@R~�@Q��@Q�^@Q��@Q��@QG�@Pr�@PA�@O�w@Ol�@O�@Nȴ@NV@M�T@M�@M`B@M?}@L�/@Lz�@L9X@L�@L�@L�@L1@Kƨ@K��@Ko@J��@J�\@JM�@I��@I�#@I�#@I�^@HĜ@HA�@H  @G�P@G�@F�@F�R@F��@F��@F�+@F5?@E�-@E`B@D��@Dj@Cƨ@Ct�@CS�@C@B^5@B�@A�^@Ax�@AG�@@Ĝ@@�9@@Ĝ@@r�@@b@?�w@?|�@?K�@?;d@?+@?
=@>�R@>�+@>@=��@=p�@=`B@=�@=V@<�@<�D@<1@;�m@;��@;��@;33@:�!@:M�@9�#@97L@8Ĝ@8�@8Q�@8  @7�w@7
=@6ȴ@6��@6E�@5�-@5?}@4�/@4�j@4�@4Z@41@3�
@3ƨ@3�F@3��@3C�@2�\@2M�@1��@1�#@1�7@1X@1&�@1�@0�`@0�u@0A�@0b@/�@/�w@/|�@/\)@/
=@.�@.��@.ff@.5?@-@-�@,�@,�/@,�/@,��@,�j@,��@,�D@,z�@,�@+ƨ@+��@+�@*�H@*��@*�\@*~�@*n�@*n�@*n�@*n�@*n�@*^5@*^5@*M�@*M�@*=q@*=q@*J@)��@)��@(��@(r�@(bN@(Q�@( �@'�@'��@'|�@'l�@'K�@&�y@&��@&v�@&�+@&��@&��@&��@&�+@&�+@&E�@%�T@%��@%��@%`B@%�@$�@$�j@$�@$�D@$Z@$I�@$9X@$�@#�
@#S�@#o@"�H@"��@"��@"~�@"=q@!��@!hs@ Ĝ@ �u@ A�@   @�w@�P@
=@�+@5?@$�@$�@{@@�-@`B@/@��@��@��@�D@Z@9X@9X@�@1@�m@�
@��@t�@�!@~�@^5@�@�^@��@�7@�7@x�@hs@X@�`@r�@Q�@Q�@A�@�@�P@l�@�@�y@�@ȴ@��@V@$�@@@�@?}@?}@��@j@I�@9X@9X@9X@(�@(�@(�@ƨ@S�@33@"�@��@�!@�!@��@��@^5@J@�@�@��@��@��@x�@X@�@�`@��@Ĝ@Ĝ@Ĝ@��@A�@  @�@�;@��@��@��@�w@�w@�@K�@+@
=@�y@�y@�y@�@�@ȴ@ȴ@��@v�@$�@��@��@�h@`B@/@�@V@��@�@�/@�/@�@Z@�@�@��@ƨ@�F@��@t�@"�@"�@o@
�@
��@
��@
�\@
^5@
J@	��@	��@	�7@	hs@	G�@	%@��@�u@1'@b@b@b@  @�@�@�@�;@��@�@��@�P@l�@;d@�y@�@�@ȴ@��@v�@�T@@�-@�h@p�@V@��@�@�@�/@��@�@j@I�@�@�@1@�@@�@�!@�!@��@�\@�\@�\@n�@�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111)A�r�A�p�A�l�A�jA�l�A�l�A�l�A�`BA�ZA�Q�A�Q�A�C�A�"�A���A��/A���A�ƨAǰ!A���AA��A���A�jA�  A�%A���A�~�A��A�x�A�9XA��A���A�\)A���A��A���A�1A���A�I�A�&�A��HA�  A��HA�XA���A�|�A�C�A���A�v�A��A��RA�C�A�\)A�jA�+A���A�z�A�$�A��\A�oA��A���A�ffA�v�A�`BA�x�A��\A�1A���A�
=A�ȴA�M�A�E�A�5?A�%A��A�+A}A{;dAw��Au�
At�RAr�/Ap��An�AmdZAl��Alv�Ak��AkoAj�Aj��Ai��Ah�`Ah^5AghsAfAd�jAd  Ac�Ab�DAbjAb-Aa/A_�;A_��A_��A_�A_oA^�`A^5?A\��A\1'A[S�AZn�AYAX^5AWXAU��AT�9AS�mAR��AQ��AQ��AQ?}AO�^AO�AOO�AO&�AOAM��AM%AL�+AL$�AJ�HAJ �AHZAG�TAGt�AFZAEl�AE&�ADffAB��AA�;AA&�A@��A?7LA>VA=��A=oA<��A<M�A;�A;%A9|�A7�A7/A6��A5��A4�A3��A2�jA2$�A1�TA1&�A0v�A0VA0�A/�PA.�HA.ffA-�A-G�A,ȴA,jA+��A+��A*��A*�+A*VA*�A)l�A(VA'��A';dA&ȴA&-A%
=A#�FA#G�A"M�A!\)A ĜA v�Al�A��A�+AA�A�-A?}A�`A�#A�^A�hA|�A`BA?}A��AXA
=AĜA�A��Az�AȴAC�A��A�9AM�A/AĜA��A`BA��AZA�/AbAt�A�A�hA�A
��A
�9A
M�A	"�A�\A�A�A��Av�A\)AA�A�A�A��A �A ^5A $�@�t�@�p�@�(�@���@�;d@��@��#@���@��!@�`B@�t�@�@��`@�@�Z@�1'@��@���@�X@�/@��@��`@�Ĝ@�u@�bN@�1'@�@�^@��@�E�@�-@���@�o@�p�@�j@�S�@�&�@�/@�j@�Z@�S�@�-@թ�@�9X@�V@Η�@��@˕�@�=q@Ɨ�@�G�@Õ�@�/@�j@��@��^@�V@�9X@�K�@��!@�@��@�l�@��y@�=q@�Ĝ@���@�"�@���@�V@�=q@��#@�X@���@�dZ@�=q@���@�
=@��H@��@��@���@���@�ȴ@���@��\@�~�@�ff@�E�@��@�p�@��;@��+@�ff@�V@�-@��-@�X@���@��9@�9X@��P@�ff@��@�bN@�(�@��m@���@�dZ@��R@�M�@��@���@���@�O�@���@�Z@��@���@���@���@�A�@���@��@��P@��@�l�@�S�@�;d@��@�@��@���@�n�@�$�@��@��@��u@�r�@�Q�@�A�@�A�@�A�@�1'@� �@�b@�  @��
@���@�|�@�;d@��y@�$�@���@���@�5?@���@���@�X@��@��@�%@��9@��u@��@�z�@�Q�@�A�@�(�@�l�@��H@�=q@�7L@��
@�33@��R@���@�ff@�=q@�$�@��@��^@���@�x�@�7L@���@�r�@� �@��@��;@��w@���@�t�@�@���@�^5@�=q@�-@�{@�@��#@���@�@��^@��-@���@��@�X@�G�@�7L@�&�@���@�r�@�b@~�R@~v�@~ff@}�@}�h@}p�@}`B@|��@{�m@{C�@{33@{33@{33@{o@z�H@z�!@y��@y�@x�@x1'@w�;@w��@w+@v��@w
=@v��@vff@u�@tj@st�@r~�@q��@qG�@p�@pA�@p1'@p �@p �@p  @o�w@o�@o|�@o\)@o;d@o�@o�@o+@o�@o
=@n��@n�@n�R@n�@n�+@n{@mp�@m�@l�/@lI�@k33@iX@f�@f��@fE�@fv�@fv�@f$�@e`B@d��@d�@c33@a��@`��@`�9@`�@`r�@`r�@`r�@`�@`bN@`b@_��@_|�@^ȴ@]��@]p�@]?}@\�D@\Z@\(�@[��@[�F@[��@[��@[t�@[dZ@["�@Z��@Z�\@Z~�@Z~�@Z~�@Zn�@Z^5@ZM�@Z=q@Z�@Yx�@X�`@XQ�@W\)@W+@W
=@W
=@V�R@V$�@Up�@UV@T��@T1@T1@T1@T1@S��@S�m@S�
@Sƨ@Sƨ@S�F@S�@SS�@S33@R~�@Q��@Q�^@Q��@Q��@QG�@Pr�@PA�@O�w@Ol�@O�@Nȴ@NV@M�T@M�@M`B@M?}@L�/@Lz�@L9X@L�@L�@L�@L1@Kƨ@K��@Ko@J��@J�\@JM�@I��@I�#@I�#@I�^@HĜ@HA�@H  @G�P@G�@F�@F�R@F��@F��@F�+@F5?@E�-@E`B@D��@Dj@Cƨ@Ct�@CS�@C@B^5@B�@A�^@Ax�@AG�@@Ĝ@@�9@@Ĝ@@r�@@b@?�w@?|�@?K�@?;d@?+@?
=@>�R@>�+@>@=��@=p�@=`B@=�@=V@<�@<�D@<1@;�m@;��@;��@;33@:�!@:M�@9�#@97L@8Ĝ@8�@8Q�@8  @7�w@7
=@6ȴ@6��@6E�@5�-@5?}@4�/@4�j@4�@4Z@41@3�
@3ƨ@3�F@3��@3C�@2�\@2M�@1��@1�#@1�7@1X@1&�@1�@0�`@0�u@0A�@0b@/�@/�w@/|�@/\)@/
=@.�@.��@.ff@.5?@-@-�@,�@,�/@,�/@,��@,�j@,��@,�D@,z�@,�@+ƨ@+��@+�@*�H@*��@*�\@*~�@*n�@*n�@*n�@*n�@*n�@*^5@*^5@*M�@*M�@*=q@*=q@*J@)��@)��@(��@(r�@(bN@(Q�@( �@'�@'��@'|�@'l�@'K�@&�y@&��@&v�@&�+@&��@&��@&��@&�+@&�+@&E�@%�T@%��@%��@%`B@%�@$�@$�j@$�@$�D@$Z@$I�@$9X@$�@#�
@#S�@#o@"�H@"��@"��@"~�@"=q@!��@!hs@ Ĝ@ �u@ A�@   @�w@�P@
=@�+@5?@$�@$�@{@@�-@`B@/@��@��@��@�D@Z@9X@9X@�@1@�m@�
@��@t�@�!@~�@^5@�@�^@��@�7@�7@x�@hs@X@�`@r�@Q�@Q�@A�@�@�P@l�@�@�y@�@ȴ@��@V@$�@@@�@?}@?}@��@j@I�@9X@9X@9X@(�@(�@(�@ƨ@S�@33@"�@��@�!@�!@��@��@^5@J@�@�@��@��@��@x�@X@�@�`@��@Ĝ@Ĝ@Ĝ@��@A�@  @�@�;@��@��@��@�w@�w@�@K�@+@
=@�y@�y@�y@�@�@ȴ@ȴ@��@v�@$�@��@��@�h@`B@/@�@V@��@�@�/@�/@�@Z@�@�@��@ƨ@�F@��@t�@"�@"�@o@
�@
��@
��@
�\@
^5@
J@	��@	��@	�7@	hs@	G�@	%@��@�u@1'@b@b@b@  @�@�@�@�;@��@�@��@�P@l�@;d@�y@�@�@ȴ@��@v�@�T@@�-@�h@p�@V@��@�@�@�/@��@�@j@I�@�@�@1@�@@�@�!@�!@��@�\@�\@�\@n�@�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111�BE�BE�BD�BD�BE�BE�BE�BE�BD�BC�BC�BB�B?}B=qB=qB=qB<jB:^B,B��B�B�B�Bm�B`BBI�BB�BE�BE�BI�BJ�BK�BK�BJ�B@�BuB�B��BBƨBǮBɺB��B�^B�?B�'B�B��B��B��B��B�\B�BffBcTB_;B[#BVBN�BG�BE�BB�B<jB,BVB  B
�B
�B
�fB
�5B
��B
ŢB
ĜB
B
�9B
��B
�PB
}�B
m�B
XB
L�B
D�B
:^B
/B
$�B
�B
�B
�B
bB
PB
DB
	7B
B	��B	��B	��B	�B	�fB	�HB	�/B	�B	�B	�
B	��B	��B	ɺB	ȴB	ȴB	ŢB	ÖB	�}B	�XB	�9B	�!B	��B	��B	��B	��B	�bB	�DB	�+B	�B	z�B	x�B	u�B	m�B	k�B	jB	iyB	hsB	dZB	]/B	XB	T�B	M�B	I�B	A�B	>wB	;dB	6FB	2-B	/B	+B	#�B	�B	�B	�B	{B	hB	\B	VB	VB	VB	
=B	B��B��B��B�B�B�B�`B�HB�;B�/B�B�
B�B��B��B��B��B��BȴBƨBĜBÖB��B�}B�qB�jB�^B�RB�9B�'B�!B�B�B��B��B��B��B��B��B��B�uB�hB�bB�\B�PB�DB�=B�+B�%B�%B�%B�B�B�B}�B|�B{�B{�Bz�Bx�Bt�Bp�Bo�Bn�Bm�BjBhsBffBe`BcTB`BB^5B[#BYBW
BT�BS�BR�BQ�BO�BM�BK�BJ�BI�BG�BF�BC�B@�B?}B=qB;dB9XB8RB7LB6FB49B33B33B2-B1'B0!B/B.B,B)�B(�B'�B'�B&�B%�B%�B$�B$�B$�B$�B$�B#�B#�B#�B"�B!�B �B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B"�B"�B$�B%�B%�B%�B'�B+B+B)�B,B+B,B0!B5?B6FB7LB8RB8RB8RB9XB:^B=qB?}BF�BH�BI�BI�BI�BI�BH�BH�BI�BI�BJ�BJ�BJ�BK�BN�BZBe`BffBffBgmBhsBiyBk�Bl�Bm�Bn�Bq�Bs�Bx�By�Bz�B|�B~�B�B�B�B�B�%B�1B�7B�DB�PB�\B�oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�-B�3B�9B�9B�9B�9B�9B�?B�?B�?B�FB�RB�XB�^B�jB��BȴB�B�#B�/B�;B�HB�TB�TB�TB�`B�fB�mB�mB�sB�sB�sB�B�B��B��B	%B	
=B	PB	VB	\B	bB	hB	oB	{B	{B	�B	�B	�B	�B	 �B	!�B	"�B	#�B	$�B	%�B	(�B	,B	-B	.B	/B	/B	0!B	1'B	2-B	2-B	33B	33B	49B	5?B	6FB	7LB	8RB	9XB	<jB	?}B	C�B	F�B	G�B	G�B	I�B	J�B	J�B	K�B	M�B	R�B	W
B	XB	XB	XB	YB	YB	ZB	^5B	`BB	cTB	cTB	dZB	dZB	e`B	e`B	e`B	ffB	ffB	ffB	ffB	ffB	ffB	ffB	ffB	hsB	iyB	iyB	iyB	jB	jB	l�B	p�B	r�B	t�B	v�B	x�B	z�B	{�B	}�B	~�B	~�B	� B	�B	�B	�1B	�7B	�JB	�JB	�PB	�PB	�JB	�DB	�DB	�JB	�VB	�\B	�bB	�hB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�'B	�3B	�9B	�LB	�LB	�RB	�XB	�^B	�^B	�^B	�dB	�dB	�jB	�wB	�}B	�}B	�}B	�}B	�}B	��B	��B	��B	��B	ÖB	ŢB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�
B	�
B	�
B	�
B	�
B	�B	�B	�B	�B	�B	�B	�B	�#B	�/B	�5B	�5B	�5B	�;B	�HB	�NB	�TB	�ZB	�`B	�fB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
%B
%B
%B
1B
1B
	7B
	7B

=B
DB
PB
PB
VB
\B
bB
bB
hB
hB
hB
hB
oB
oB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
"�B
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
(�B
+B
+B
,B
,B
,B
-B
-B
-B
-B
.B
/B
/B
/B
/B
0!B
0!B
1'B
1'B
1'B
2-B
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
6FB
6FB
6FB
7LB
8RB
8RB
8RB
8RB
8RB
8RB
8RB
8RB
8RB
8RB
8RB
8RB
8RB
8RB
9XB
9XB
9XB
;dB
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
>wB
?}B
?}B
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
B�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
E�B
F�B
F�B
G�B
G�B
G�B
G�B
H�B
I�B
J�B
J�B
J�B
J�B
J�B
J�B
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
O�B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
P�B
Q�B
P�B
Q�B
R�B
R�B
R�B
R�B
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
W
B
W
B
W
B
W
B
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
ZB
ZB
ZB
[#B
[#B
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
`BB
_;B
_;B
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
aHB
bNB
bNB
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
gmB
gmB
gmB
hsB
hsB
iyB
iyB
iyB
iyB
iyB
jB
jB
jB
k�B
k�B
k�B
k�B
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
m�B
m�B
m�B
m�B
m�B
m�B
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
p�B
p�B
q�B
q�B
q�B
q�B
q�B
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
t�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111�BE�BE�BD�BD�BE�BE�BE�BE�BD�BC�BC�BB�B?�B=�B=�B=�B=�B>BB4�B�B��B��B��B{Bg�BLJBC�BFtBFYBJrBK�BL�BM�BN�BH�BB��B��B�-B��B��B��B��B��B��B��B��B�B��B��B�B��B��BgRBd&B`'B\BWsBO�BH1BFtBC�B?HB0;B�B�B
�B
��B
�$B
��B
̳B
�B
ŢB
�mB
�fB
��B
��B
�UB
q[B
Z7B
N�B
GB
<�B
1AB
&�B
]B
KB
mB
 B
�B
�B

XB
B	��B	�VB	�fB	�B	�mB	�NB	��B	�kB	ٴB	�_B	� B	�)B	��B	�B	�7B	�%B	ĶB	�B	��B	��B	�vB	��B	��B	�BB	�kB	��B	�dB	��B	�B	{dB	y�B	wLB	m�B	k�B	j�B	i�B	i�B	ezB	^B	YB	V�B	O(B	K�B	B[B	?HB	<�B	7fB	2�B	0oB	,�B	$�B	�B	�B	7B	�B	TB	B	B	�B	\B	^B	B	 �B��B��B��B�'B�B�B�B��B�B��B�YBևB��B��BЗBΊB̘BɆB�EB�SB�3B�[B�4B��B�B�B��B�%B��B��B�5B��B�sB��B�B��B�kB�EB��B�,B�B��B�.B�"B��B�^B�zB�tB�YB�tB��B�B��B~�B}VB|PB|6B{�Bz�BvzBq'BpUBoiBn�BkQBi�BgRBf2Bd�BbB_pB\)BZQBX_BU�BT{BS[BR�BQhBN�BL�BLBJ�BHfBH1BEBBB@�B>�B<PB:*B8�B8B7�B5B3�B3�B2�B2B1�B0B/OB-]B+B)�B(>B(XB'mB&�B&�B%FB%B%B%B%B$&B$@B$@B#�B#:B"B �B \B�B!B�B�B�B]B�B=BBkBkBQBB�BeBBQBB�B�B B!HB#�B#�B%�B&�B&�B&�B(�B+�B,B*�B,�B+�B-B0�B5�B6�B7�B8�B8�B8�B:B;dB>wBABG+BIBI�BI�BI�BI�BH�BIBI�BJ	BJ�BJ�BKDBL�BO�BZ�BezBf�Bf�Bg�Bh�Bi�Bk�BmBn/Bo�Br|Bt�By	Bz*B{0B}<B}B�aB�MB�SB��B�tB��B��B��B��B�HB��B�	B��B�B��B��B�B�B�B�&B�,B�B�2B�$B�XB�DB��B��B�GB�MB�nB�nB�TB�TB�TB�ZB�ZB�tB�zB��B��B��B�<B��B��BևB�WB�~BߊB�|B�nB�nB�B�zB�B�B�B�B��B�*B�B�[B��B��B	�B	
�B	jB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	B	 �B	!�B	#B	$B	%B	&LB	)DB	,=B	-)B	.IB	/5B	/5B	0UB	1[B	2GB	2GB	3MB	3MB	4nB	5ZB	6`B	7fB	8�B	9�B	<�B	?�B	C�B	F�B	G�B	G�B	J	B	J�B	J�B	LB	N"B	S&B	W
B	X+B	X+B	XEB	Y1B	YKB	Z�B	^�B	`�B	cnB	c�B	dtB	d�B	e�B	e�B	e�B	f�B	f�B	f�B	f�B	f�B	f�B	f�B	f�B	h�B	i�B	i�B	i�B	j�B	j�B	l�B	p�B	r�B	t�B	v�B	x�B	z�B	|B	~B	B	B	�B	�B	�MB	�fB	��B	�~B	�~B	��B	��B	�B	�B	�^B	�dB	�VB	�vB	��B	��B	��B	��B	��B	�B	�'B	��B	��B	��B	��B	��B	��B	��B	�B	�$B	�*B	�KB	�cB	�vB	�MB	��B	��B	�fB	�lB	�rB	��B	�xB	�xB	�B	�B	��B	��B	�}B	��B	�}B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	�B	�B	�B	�:B	�@B	�2B	�$B	�
B	�$B	�$B	�$B	�$B	�B	�B	�EB	�+B	�+B	�1B	�B	�WB	�IB	�OB	�jB	�jB	ߊB	�bB	�B	�nB	�tB	�B	�B	�B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	��B	�B	��B	��B	��B	��B	��B	��B	�B	�B	��B	�B	�B	�B	�B	��B	��B	�B	�"B	�(B	�HB
;B
AB
GB
9B
?B
YB
tB
KB
fB
	RB
	lB

rB
xB
PB
jB
�B
vB
}B
}B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
 �B
!�B
!�B
!�B
# B
#�B
$�B
$�B
%�B
&B
'B
'B
(
B
(
B
(>B
)DB
+B
+6B
,"B
,"B
,"B
-)B
-)B
-)B
-)B
./B
/OB
/5B
/OB
/OB
0;B
0;B
1AB
1AB
1AB
2aB
2aB
3hB
4TB
49B
5?B
5ZB
5ZB
5tB
5tB
5ZB
5tB
5tB
6`B
6`B
6zB
7fB
8lB
8RB
8RB
8RB
8lB
8RB
8RB
8lB
8RB
8lB
8RB
8RB
8RB
8lB
9�B
9�B
9�B
;B
;B
;B
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
>�B
>wB
>�B
>wB
>�B
>�B
>�B
?�B
?�B
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
B�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
E�B
F�B
F�B
G�B
G�B
G�B
G�B
H�B
I�B
J�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
L�B
MB
L�B
L�B
L�B
M�B
M�B
M�B
NB
M�B
N<B
PB
PB
O�B
PB
Q B
Q B
P�B
P�B
QB
RB
Q4B
RB
R�B
R�B
SB
S@B
T,B
TB
TB
UB
T�B
UB
UB
U2B
VB
VB
VB
W?B
W$B
W$B
WYB
X+B
YB
Y1B
YB
YB
YB
YB
Y1B
YKB
YeB
Z7B
Z7B
ZQB
[#B
[#B
[#B
[=B
[=B
\CB
\CB
\CB
\CB
]/B
]IB
]IB
]dB
]IB
^OB
^5B
^5B
^OB
^5B
^OB
^jB
_VB
_;B
_VB
_;B
_;B
_VB
`BB
_;B
_VB
`\B
`vB
`\B
`\B
aHB
aHB
aHB
abB
abB
aHB
abB
abB
abB
b�B
bhB
cTB
cnB
cnB
cTB
cTB
cnB
dZB
dtB
dZB
dtB
dtB
ezB
e`B
ezB
ezB
ffB
f�B
f�B
f�B
g�B
g�B
g�B
g�B
g�B
g�B
g�B
h�B
h�B
i�B
i�B
i�B
i�B
i�B
j�B
j�B
j�B
k�B
k�B
k�B
k�B
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
m�B
m�B
m�B
m�B
m�B
m�B
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
p�B
p�B
q�B
q�B
q�B
q�B
q�B
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
t�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111�@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<,1<#�
<XD�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.05(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201808170036372018081700363720180817003637202211182135462022111821354620221118213546201808180020322018081800203220180818002032  JA  ARFMdecpA19c                                                                20180807003511  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180806153512  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180806153514  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180806153514  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180806153515  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180806153515  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180806153515  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180806153515  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180806153515  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180806153515                      G�O�G�O�G�O�                JA  ARUP                                                                        20180806155551                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180806153437  CV  JULD            G�O�G�O�Fý                JM  ARCAJMQC2.0                                                                 20180816153637  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180816153637  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180817152032  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200114171536                      G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20221118123546  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20221123111507                      G�O�G�O�G�O�                