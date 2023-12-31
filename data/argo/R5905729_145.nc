CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2022-04-16T09:02:37Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
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
resolution        >�EȠ      
_FillValue        A.�~       axis      T           80   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    88   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
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
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  `�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  p    TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �H   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �0   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �p   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �X   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ݀   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ݰ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �,   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �<   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �@   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �P   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �T   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �X   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �\Argo profile    3.1 1.2 19500101000000  20220416090237  20220416090237  5905729 Argo PMEL                                                       GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               �A   AO  7160                            2B  A   NAVIS_A                         0838                            170425                          863 @��ի<X)1   @���I��V@&�bM���dH1&�y1   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         �A   B   B   @9��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�33B���B�  B�33B���B�33B���B�  B���B���B�  B�  B�  B�  B�  B�33B�  B���B���B�  B�  B�  B�  B�  B�  B���B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`�Cb  Cd  Cf  Ch  Cj  Cl�Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DGy�DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dzy�D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D���D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@6fg@|��@�ff@�ffA33A?33A_33A33A���A���A���A���Aϙ�Aߙ�AA���B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B��fB��fB��fB��fB��B�� B��fB��B�� B��B��3B��fB��3B��3B��fB��fB��fB��fB��fB��B��fB׳3B۳3B��fB��fB��fB��fB��fB��fB��3B��fB��fC�3C�3C�3C�3C	�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C!�3C#�3C%�3C'�3C)�3C+�3C-�3C/�3C1�3C3�3C5�3C7�3C9�3C;�3C=�3C?�3CA�3CC�3CE�3CG�3CI�3CK�3CM�3CO�3CQ�3CS�3CU�3CW�3CY�3C[�3C]�3C`�Ca�3Cc�3Ce�3Cg�3Ci�3Cl�Cm�3Co�3Cq�3Cs�3Cu�3Cw�3Cy�3C{�3C}�3C�3C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D |�D ��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D	|�D	��D
|�D
��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D |�D ��D!|�D!��D"|�D"��D#|�D#��D$|�D$��D%|�D%��D&|�D&��D'|�D'��D(|�D(��D)|�D)��D*|�D*��D+|�D+��D,|�D,��D-|�D-��D.|�D.��D/|�D/��D0|�D0��D1|�D1��D2|�D2��D3|�D3��D4|�D4��D5|�D5��D6|�D6��D7|�D7��D8|�D8��D9|�D9��D:|�D:��D;|�D;��D<|�D<��D=|�D=��D>|�D>��D?|�D?��D@|�D@��DA|�DA��DB|�DB��DC|�DC��DD|�DD��DE|�DE��DF|�DF��DGvgDG��DH|�DH��DI|�DI��DJ|�DJ��DK|�DK��DL|�DL��DM|�DM��DN|�DN��DO|�DO��DP|�DP��DQ|�DQ��DR|�DR��DS|�DS��DT|�DT��DU|�DU��DV|�DV��DW|�DW��DX|�DX��DY|�DY��DZ|�DZ��D[|�D[��D\|�D\��D]|�D]��D^|�D^��D_|�D_��D`|�D`��Da|�Da��Db|�Db��Dc|�Dc��Dd|�Dd��De|�De��Df|�Df��Dg|�Dg��Dh|�Dh��Di|�Di��Dj|�Dj��Dk|�Dk��Dl|�Dl��Dm|�Dm��Dn|�Dn��Do|�Do��Dp|�Dp��Dq|�Dq��Dr|�Dr��Ds|�Ds��Dt|�Dt��Du|�Du��Dv|�Dv��Dw|�Dw��Dx|�Dx��Dy|�Dy��DzvgDz��D{|�D{��D||�D|��D}|�D}��D~|�D~��D|�D��D�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��D�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD���D��D�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD¾fD��fD�>fD�~fDþfD��fD�>fD�~fDľfD��fD�>fD�~fDžfD��fD�>fD�~fDƾfD��fD�>fD�~fDǾfD��fD�>fD�~fDȾfD��fD�>fD�~fDɾfD��fD�>fD�~fDʾfD��fD�>fD�~fD˾fD��fD�>fD�~fD̾fD��fD�>fD�~fD;fD��fD�>fD�~fDξfD��fD�>fD�~fDϾfD��fD�>fD�~fDоfD��fD�>fD�~fDѾfD��fD�>fD�~fDҾfD��fD�>fD�~fDӾfD��fD�>fD�~fDԾfD��fD�>fD�~fDվfD��fD�>fD�~fD־fD��fD�>fD�~fD׾fD��fD�>fD�~fDؾfD��fD�>fD�~fDپfD��fD�>fD�~fDھfD��fD�>fD�~fD۾fD��3D�>fD�~fDܾfD��fD�>fD�~fDݾfD��fD�>fD�~fD޾fD��fD�>fD�~fD߾fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD��fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD���D��31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�5?A�1'A�1'A�33A�33A�33A�5?A�5?A�$�A� �A��A��A�JA�  A�  A�A��A��A��
A��
A��A��
A���A���A���A���A���A���A��A��/A��;A��HA��HA��HA��/A��;A�z�AոRA�oA�
=A���A�1A�A��^A���A�"�A��DA��#A���A�Q�A�t�A�\)A�JA���A�~�A�p�A�\)A�7LA��A�S�A���A�t�A�5?A��A�ĜA��FA�v�A�%A~�uAwx�Arn�AnffAghsAa33A^AZ��AS�#APv�AP��APffALz�AI��AH�/AGAE�AB��A@v�A@^5A?��A?��A>��A>�9A>�DA>1'A=�wA=�7A=+A<�!A<~�A<�A;�-A;K�A;%A:ffA:bA9�;A9G�A8jA7�A7K�A7%A6�DA6$�A5�A5�PA533A4�`A4��A41A3VA2n�A1
=A/t�A.(�A-|�A,��A+?}A*bNA)ƨA)/A(�RA(Q�A({A'�TA'��A'O�A'�A'VA&��A%��A%��A%�PA%|�A%\)A$�9A$v�A$�A#��A#ƨA#K�A"��A"��A"r�A"$�A!��A!�hA!C�A!/A ��A I�A �A�A�A�A�A=qA��AdZA+A��AĜA�!A�A�A�A?}A��A9XAƨA\)A~�A�;A��A��Ax�A"�A�DA9XA�mA�PA�AA�A��A%A��A~�A$�A�AE�A$�A �A�A�A�AdZA"�A��A��A��A�-A��A��A�hA�Al�A;dA��A�A�!A�+AffA�AdZA
ȴA
n�A
^5A	�A	�7A	hsA	XA��AffA��AK�A"�A��A��AI�AA?}A�`A��A~�Ar�AbNAI�A5?A�A��AXA%AbNA��A?}A �`A �RA {@��T@��7@�x�@�`B@�7L@��`@���@�j@�1'@�(�@��
@�l�@��H@�^5@�=q@���@���@���@��9@�bN@��F@�K�@���@��R@�ff@��h@�bN@�+@�!@�ff@�M�@�-@��#@�G�@��@�Z@�1@��;@@�l�@�+@�ff@홚@�Ĝ@�  @�E�@��@���@陚@�O�@�Ĝ@�ƨ@�\@��T@��@�p�@�X@���@�Ĝ@�@�j@�F@�@�33@���@�J@���@�;d@��@�v�@��@�Ĝ@�K�@��@�7L@�A�@׾w@�@��@��y@�ȴ@ְ!@�~�@ա�@ԋD@җ�@���@���@ѡ�@�7L@�bN@��y@�hs@̬@�1'@ˮ@�"�@��@Ɂ@�G�@�ƨ@�o@�=q@�O�@���@ēu@�j@�(�@��
@þw@å�@�\)@�o@��@+@���@��@�I�@��@�\)@�"�@��@���@�V@��@���@���@�hs@��@�I�@�9X@�1@��m@�ƨ@���@�\)@�n�@�x�@�7L@�&�@��@��/@�r�@��@��@��w@�|�@��@�ȴ@�n�@�5?@���@�G�@�r�@�9X@�ƨ@�;d@��!@��#@�x�@�7L@�%@���@�z�@���@�ȴ@��\@�-@��-@�&�@���@�I�@�b@��P@�C�@��@�V@���@��@�x�@�%@� �@��m@���@��@��\@��#@�O�@���@�A�@��;@�|�@���@�~�@�$�@��h@��@��j@�(�@��@��@�^5@���@�?}@���@���@�+@�v�@���@�x�@�%@���@�bN@� �@���@���@�o@�ȴ@���@��!@���@�V@��#@��@�7L@��@��9@�bN@��@��@��w@�|�@��H@��+@�v�@�ff@�E�@�$�@�@��#@��7@��@���@�Ĝ@��D@�9X@��;@��@�t�@�l�@�+@�o@��@���@���@��+@�-@�@��@��@��@��-@�`B@�?}@�/@��@�%@��@���@��9@�j@�A�@� �@���@��F@�"�@��+@���@��-@���@�p�@�V@��@�z�@�Z@��@��
@��@�dZ@�+@���@���@�V@���@�?}@���@�Ĝ@��@��@�Q�@�b@��
@�l�@��@���@���@��+@�^5@�E�@�=q@�=q@�=q@�=q@�=q@�=q@�-@��#@�/@��9@�9X@��@+@~v�@~{@}��@|��@|Z@{ƨ@{��@{t�@{t�@{t�@{"�@z��@z�@y7L@w�;@w�@u�T@t�@t9X@sC�@r�!@r�\@rn�@r^5@r^5@r-@q�@pĜ@p  @o�@n�R@n��@n�+@nff@n{@m@l��@l1@k�F@k�@kdZ@k"�@jn�@j-@ix�@iX@iG�@i7L@i%@h �@g��@g�@f��@f$�@e�T@e/@d�@dj@dI�@c��@b�@b��@b�@ahs@`��@`�@`Q�@`  @_�;@_��@_�w@_�P@_+@_�@^��@^ff@]�T@]`B@\�@\�@[ƨ@[C�@[@Z�H@Z�!@Zn�@ZJ@Y�#@Y��@Yx�@Y&�@XĜ@XbN@X1'@Xb@W��@W+@W
=@W
=@V��@VE�@V5?@V$�@V@Up�@U/@T��@T��@T�j@T�@T�D@T�D@T(�@Sƨ@SS�@R�\@R=q@RJ@Q�@Q��@Q&�@PĜ@P�@PQ�@P1'@P  @O��@O�w@O�P@O\)@O+@Nȴ@Nv�@N5?@N$�@N{@M�T@M�@L��@Lz�@L�@K�m@K�F@K��@K��@Kt�@K33@J��@JM�@I�@I��@I�^@I��@Ihs@IG�@I�@I%@H��@H�9@H�u@H �@G�@G��@G�P@G|�@F��@F$�@E�@D��@D��@DI�@C��@Ct�@CdZ@CS�@C33@B�H@B��@B~�@BJ@A�#@A��@A&�@@��@@�@?�w@?
=@>�+@>ff@>E�@=�@=�@=�@<z�@<Z@<1@;��@;C�@:��@:~�@:-@9��@9�#@9hs@9�@8Ĝ@8A�@8b@7�;@7��@7
=@65?@5�T@5��@5@5�-@5�h@5`B@5/@4�@4�D@49X@4(�@4�@4�@3�
@333@2�@2��@2=q@2�@2�@2�@2J@1x�@0�@0r�@0Q�@/�@/;d@/+@/+@.�R@.$�@-�-@-/@,�@,�j@,��@,Z@+ƨ@+dZ@+"�@+@*J@)7L@)%@(��@(Q�@'�w@';d@&�+@%@%�@%/@%V@$��@%�@%V@$�j@$�D@$j@$I�@$�@#ƨ@#��@#C�@"��@"��@"�\@"-@!��@!�7@!G�@!�@ ��@ ��@ Q�@ b@�w@K�@
=@��@��@��@��@5?@��@�-@�h@`B@�@�j@��@Z@9X@(�@1@�
@�
@��@S�@33@@��@�!@~�@-@J@��@��@hs@�@��@�`@�`@��@�@1'@  @�;@�w@�@�P@|�@l�@\)@K�@+@
=@�R@E�@$�@$�@��@��@�h@p�@p�@`B@O�@�@�@�/@z�@(�@��@��@@��@n�@M�@M�@=q@-@�@��@hs@X@G�@�@��@�@�@r�@r�@r�@r�@bN@bN@Q�@A�@  @|�@K�@K�@K�@K�@+@
=@
=@
=@
=@��@�@�+@ff@V@$�@�@��@�h@O�@/@/@�@�@�@V@V@�/@�j@��@�D@Z@9X@�@��@�m@�m@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144A�5?A�1'A�1'A�33A�33A�33A�5?A�5?A�$�A� �A��A��A�JA�  A�  A�A��A��A��
A��
A��A��
A���A���A���A���A���A���A��A��/A��;A��HA��HA��HA��/A��;A�z�AոRA�oA�
=A���A�1A�A��^A���A�"�A��DA��#A���A�Q�A�t�A�\)A�JA���A�~�A�p�A�\)A�7LA��A�S�A���A�t�A�5?A��A�ĜA��FA�v�A�%A~�uAwx�Arn�AnffAghsAa33A^AZ��AS�#APv�AP��APffALz�AI��AH�/AGAE�AB��A@v�A@^5A?��A?��A>��A>�9A>�DA>1'A=�wA=�7A=+A<�!A<~�A<�A;�-A;K�A;%A:ffA:bA9�;A9G�A8jA7�A7K�A7%A6�DA6$�A5�A5�PA533A4�`A4��A41A3VA2n�A1
=A/t�A.(�A-|�A,��A+?}A*bNA)ƨA)/A(�RA(Q�A({A'�TA'��A'O�A'�A'VA&��A%��A%��A%�PA%|�A%\)A$�9A$v�A$�A#��A#ƨA#K�A"��A"��A"r�A"$�A!��A!�hA!C�A!/A ��A I�A �A�A�A�A�A=qA��AdZA+A��AĜA�!A�A�A�A?}A��A9XAƨA\)A~�A�;A��A��Ax�A"�A�DA9XA�mA�PA�AA�A��A%A��A~�A$�A�AE�A$�A �A�A�A�AdZA"�A��A��A��A�-A��A��A�hA�Al�A;dA��A�A�!A�+AffA�AdZA
ȴA
n�A
^5A	�A	�7A	hsA	XA��AffA��AK�A"�A��A��AI�AA?}A�`A��A~�Ar�AbNAI�A5?A�A��AXA%AbNA��A?}A �`A �RA {@��T@��7@�x�@�`B@�7L@��`@���@�j@�1'@�(�@��
@�l�@��H@�^5@�=q@���@���@���@��9@�bN@��F@�K�@���@��R@�ff@��h@�bN@�+@�!@�ff@�M�@�-@��#@�G�@��@�Z@�1@��;@@�l�@�+@�ff@홚@�Ĝ@�  @�E�@��@���@陚@�O�@�Ĝ@�ƨ@�\@��T@��@�p�@�X@���@�Ĝ@�@�j@�F@�@�33@���@�J@���@�;d@��@�v�@��@�Ĝ@�K�@��@�7L@�A�@׾w@�@��@��y@�ȴ@ְ!@�~�@ա�@ԋD@җ�@���@���@ѡ�@�7L@�bN@��y@�hs@̬@�1'@ˮ@�"�@��@Ɂ@�G�@�ƨ@�o@�=q@�O�@���@ēu@�j@�(�@��
@þw@å�@�\)@�o@��@+@���@��@�I�@��@�\)@�"�@��@���@�V@��@���@���@�hs@��@�I�@�9X@�1@��m@�ƨ@���@�\)@�n�@�x�@�7L@�&�@��@��/@�r�@��@��@��w@�|�@��@�ȴ@�n�@�5?@���@�G�@�r�@�9X@�ƨ@�;d@��!@��#@�x�@�7L@�%@���@�z�@���@�ȴ@��\@�-@��-@�&�@���@�I�@�b@��P@�C�@��@�V@���@��@�x�@�%@� �@��m@���@��@��\@��#@�O�@���@�A�@��;@�|�@���@�~�@�$�@��h@��@��j@�(�@��@��@�^5@���@�?}@���@���@�+@�v�@���@�x�@�%@���@�bN@� �@���@���@�o@�ȴ@���@��!@���@�V@��#@��@�7L@��@��9@�bN@��@��@��w@�|�@��H@��+@�v�@�ff@�E�@�$�@�@��#@��7@��@���@�Ĝ@��D@�9X@��;@��@�t�@�l�@�+@�o@��@���@���@��+@�-@�@��@��@��@��-@�`B@�?}@�/@��@�%@��@���@��9@�j@�A�@� �@���@��F@�"�@��+@���@��-@���@�p�@�V@��@�z�@�Z@��@��
@��@�dZ@�+@���@���@�V@���@�?}@���@�Ĝ@��@��@�Q�@�b@��
@�l�@��@���@���@��+@�^5@�E�@�=q@�=q@�=q@�=q@�=q@�=q@�-@��#@�/@��9@�9X@��@+@~v�@~{@}��@|��@|Z@{ƨ@{��@{t�@{t�@{t�@{"�@z��@z�@y7L@w�;@w�@u�T@t�@t9X@sC�@r�!@r�\@rn�@r^5@r^5@r-@q�@pĜ@p  @o�@n�R@n��@n�+@nff@n{@m@l��@l1@k�F@k�@kdZ@k"�@jn�@j-@ix�@iX@iG�@i7L@i%@h �@g��@g�@f��@f$�@e�T@e/@d�@dj@dI�@c��@b�@b��@b�@ahs@`��@`�@`Q�@`  @_�;@_��@_�w@_�P@_+@_�@^��@^ff@]�T@]`B@\�@\�@[ƨ@[C�@[@Z�H@Z�!@Zn�@ZJ@Y�#@Y��@Yx�@Y&�@XĜ@XbN@X1'@Xb@W��@W+@W
=@W
=@V��@VE�@V5?@V$�@V@Up�@U/@T��@T��@T�j@T�@T�D@T�D@T(�@Sƨ@SS�@R�\@R=q@RJ@Q�@Q��@Q&�@PĜ@P�@PQ�@P1'@P  @O��@O�w@O�P@O\)@O+@Nȴ@Nv�@N5?@N$�@N{@M�T@M�@L��@Lz�@L�@K�m@K�F@K��@K��@Kt�@K33@J��@JM�@I�@I��@I�^@I��@Ihs@IG�@I�@I%@H��@H�9@H�u@H �@G�@G��@G�P@G|�@F��@F$�@E�@D��@D��@DI�@C��@Ct�@CdZ@CS�@C33@B�H@B��@B~�@BJ@A�#@A��@A&�@@��@@�@?�w@?
=@>�+@>ff@>E�@=�@=�@=�@<z�@<Z@<1@;��@;C�@:��@:~�@:-@9��@9�#@9hs@9�@8Ĝ@8A�@8b@7�;@7��@7
=@65?@5�T@5��@5@5�-@5�h@5`B@5/@4�@4�D@49X@4(�@4�@4�@3�
@333@2�@2��@2=q@2�@2�@2�@2J@1x�@0�@0r�@0Q�@/�@/;d@/+@/+@.�R@.$�@-�-@-/@,�@,�j@,��@,Z@+ƨ@+dZ@+"�@+@*J@)7L@)%@(��@(Q�@'�w@';d@&�+@%@%�@%/@%V@$��@%�@%V@$�j@$�D@$j@$I�@$�@#ƨ@#��@#C�@"��@"��@"�\@"-@!��@!�7@!G�@!�@ ��@ ��@ Q�@ b@�w@K�@
=@��@��@��@��@5?@��@�-@�h@`B@�@�j@��@Z@9X@(�@1@�
@�
@��@S�@33@@��@�!@~�@-@J@��@��@hs@�@��@�`@�`@��@�@1'@  @�;@�w@�@�P@|�@l�@\)@K�@+@
=@�R@E�@$�@$�@��@��@�h@p�@p�@`B@O�@�@�@�/@z�@(�@��@��@@��@n�@M�@M�@=q@-@�@��@hs@X@G�@�@��@�@�@r�@r�@r�@r�@bN@bN@Q�@A�@  @|�@K�@K�@K�@K�@+@
=@
=@
=@
=@��@�@�+@ff@V@$�@�@��@�h@O�@/@/@�@�@�@V@V@�/@�j@��@�D@Z@9X@�@��@�m@�m@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
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
��B
��B
��B
�B
�3B
�B
�^B
`BB
�B�\B�'B��B�bB��B�LB��B�B;dB
�B
��B
��B
�RB
B
ƨB
�'B
�uB
n�B
aHB
`BB
]/B
P�B
H�B
�B	��B	��B	��B	�hB	�\B	H�B	P�B	^5B	Q�B	G�B	T�B	��B	��B	�7B	�3B	��B	��B	��B	�qB	��B
  B
%B
�B
 �B
.B
5?B
;dB
<jB
@�B
>wB
@�B
K�B
G�B
I�B
L�B
T�B
R�B
\)B
_;B
\)B
\)B
e`B
t�B
u�B
u�B
z�B
�B
� B
�B
�=B
�JB
�1B
�=B
�\B
�VB
��B
��B
�!B
�B
��B
��B
�B
�B
�B
�3B
�?B
�FB
�9B
�FB
�LB
�^B
�?B
�!B
�dB
�qB
�jB
�dB
�9B
�jB
�XB
�^B
�XB
�?B
�?B
�RB
�?B
�FB
�LB
�3B
�-B
�FB
�B
�!B
�!B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
��B
��B
��B
��B
��B
��B
��B
��B
�\B
�bB
��B
��B
�{B
�bB
�DB
�JB
�DB
�+B
|�B
�B
}�B
}�B
� B
�B
{�B
n�B
x�B
}�B
~�B
}�B
z�B
w�B
u�B
u�B
s�B
r�B
n�B
t�B
y�B
y�B
x�B
w�B
u�B
s�B
r�B
u�B
t�B
q�B
n�B
e`B
^5B
^5B
`BB
cTB
]/B
]/B
`BB
_;B
YB
P�B
S�B
P�B
XB
XB
P�B
P�B
J�B
L�B
N�B
P�B
R�B
T�B
S�B
Q�B
P�B
L�B
I�B
H�B
E�B
>wB
?}B
?}B
A�B
A�B
9XB
1'B
C�B
F�B
F�B
E�B
C�B
B�B
C�B
C�B
C�B
A�B
>wB
>wB
=qB
A�B
?}B
=qB
;dB
=qB
<jB
8RB
:^B
9XB
8RB
6FB
1'B
.B
0!B
5?B
8RB
8RB
7LB
49B
1'B
49B
0!B
33B
49B
2-B
2-B
/B
(�B
'�B
&�B
%�B
"�B
.B
0!B
/B
,B
&�B
"�B
"�B
(�B
-B
/B
/B
-B
-B
-B
)�B
&�B
)�B
'�B
$�B
�B
�B
�B
"�B
!�B
�B
�B
hB
�B
�B
�B
 �B
 �B
&�B
&�B
%�B
$�B
"�B
�B
�B
�B
 �B
%�B
$�B
 �B
�B
{B
�B
�B
 �B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
"�B
#�B
!�B
�B
�B
�B
"�B
!�B
 �B
�B
!�B
"�B
#�B
"�B
#�B
"�B
"�B
"�B
$�B
&�B
#�B
�B
$�B
%�B
"�B
"�B
!�B
#�B
%�B
#�B
&�B
&�B
&�B
'�B
(�B
'�B
'�B
(�B
&�B
'�B
%�B
%�B
%�B
(�B
&�B
$�B
)�B
)�B
+B
/B
/B
1'B
0!B
1'B
2-B
0!B
0!B
2-B
5?B
49B
33B
1'B
/B
1'B
33B
33B
49B
33B
33B
49B
49B
33B
0!B
49B
8RB
8RB
7LB
7LB
7LB
6FB
5?B
2-B
9XB
9XB
7LB
6FB
7LB
7LB
;dB
;dB
:^B
;dB
;dB
;dB
;dB
;dB
:^B
;dB
=qB
=qB
<jB
:^B
9XB
=qB
=qB
=qB
=qB
<jB
<jB
;dB
:^B
;dB
;dB
9XB
7LB
49B
5?B
5?B
>wB
>wB
<jB
:^B
;dB
>wB
>wB
=qB
=qB
?}B
=qB
>wB
=qB
>wB
>wB
<jB
:^B
?}B
D�B
C�B
B�B
B�B
A�B
A�B
@�B
B�B
D�B
E�B
H�B
H�B
I�B
J�B
J�B
J�B
I�B
H�B
G�B
E�B
A�B
>wB
A�B
B�B
E�B
G�B
F�B
I�B
H�B
G�B
J�B
K�B
M�B
O�B
N�B
N�B
L�B
L�B
I�B
I�B
G�B
J�B
J�B
K�B
N�B
N�B
Q�B
VB
VB
VB
VB
S�B
P�B
S�B
R�B
R�B
W
B
YB
XB
XB
VB
VB
T�B
S�B
YB
ZB
ZB
YB
W
B
ZB
YB
\)B
]/B
\)B
[#B
XB
ZB
[#B
[#B
[#B
\)B
[#B
\)B
^5B
]/B
\)B
[#B
^5B
\)B
\)B
]/B
aHB
aHB
aHB
bNB
bNB
bNB
aHB
`BB
bNB
aHB
^5B
_;B
_;B
_;B
`BB
bNB
bNB
cTB
e`B
dZB
dZB
cTB
e`B
e`B
ffB
e`B
e`B
dZB
ffB
ffB
e`B
dZB
gmB
gmB
ffB
ffB
hsB
hsB
gmB
e`B
gmB
hsB
hsB
iyB
iyB
hsB
hsB
ffB
e`B
ffB
dZB
hsB
iyB
iyB
iyB
hsB
hsB
jB
jB
jB
jB
k�B
k�B
jB
jB
jB
iyB
jB
jB
l�B
k�B
jB
iyB
hsB
iyB
jB
m�B
m�B
n�B
m�B
m�B
k�B
jB
k�B
m�B
n�B
o�B
n�B
n�B
o�B
o�B
o�B
o�B
n�B
n�B
m�B
l�B
o�B
o�B
m�B
jB
hsB
iyB
n�B
o�B
n�B
m�B
r�B
r�B
r�B
r�B
p�B
q�B
o�B
p�B
q�B
p�B
p�B
p�B
o�B
m�B
n�B
q�B
s�B
s�B
r�B
q�B
r�B
p�B
t�B
s�B
s�B
s�B
s�B
v�B
v�B
v�B
v�B
u�B
v�B
v�B
u�B
v�B
w�B
v�B
t�B
t�B
x�B
{�B
{�B
z�B
z�B
y�B
z�B
y�B
y�B
z�B
|�B
{�B
{�B
y�B
w�B
y�B
z�B
z�B
|�B
}�B
|�B
{�B
x�B
w�B
}�B
|�B
z�B
{�B
~�B
}�B
{�B
z�B
|�B
|�B
~�B
~�B
~�B
}�B
|�B
|�B
~�B
}�B
x�B
y�B
~�B
~�B
|�B
|�B
|�B
~�B
~�B
�B
�B
�B
�%B
�+B
�+B
�%B
�%B
�1B
�+B
�+B
�%B
�+B
�%B
�B
�+B
�1B
�%B
�%B
�+B
�+B
�+B
�+B
�+B
�+B
�+B
�%B
�%B
�+B
�1B
�=B
�=B
�7B
�1B
�1B
�=B
�=B
�7B
�7B
�=B
�DB
�DB
�JB
�PB
�JB
�JB
�PB
�PB
�PB
�VB
�VB
�PB
�VB
�VB
�PB
�VB
�VB
�\B
�\B
�\B
�bB
�hB
�hB
�bB
�\B
�\B
�bB
�hB
�hB
�oB
�oB
�oB
�oB
�oB
�oB
�oB
�oB
�hB
�hB
�uB
�uB
�oB
�uB
�{B
�{B
�{B
�{B
�uB
�uB
�uB
�uB
�oB
�oB
�uB
�uB
�oB
�{B
�{B
��B
��B
��B
��B
��B
�{B
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
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B	C�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144B
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
��B
��B
��B
�B
�3B
�B
�^B
`BB
�B�\B�'B��B�bB��B�LB��B�B;dB
�B
��B
��B
�RB
B
ƨB
�'B
�uB
n�B
aHB
`BB
]/B
P�B
H�B
�B	��B	��B	��B	�hB	�\B	H�B	P�B	^5B	Q�B	G�B	T�B	��B	��B	�7B	�3B	��B	��B	��B	�qB	��B
  B
%B
�B
 �B
.B
5?B
;dB
<jB
@�B
>wB
@�B
K�B
G�B
I�B
L�B
T�B
R�B
\)B
_;B
\)B
\)B
e`B
t�B
u�B
u�B
z�B
�B
� B
�B
�=B
�JB
�1B
�=B
�\B
�VB
��B
��B
�!B
�B
��B
��B
�B
�B
�B
�3B
�?B
�FB
�9B
�FB
�LB
�^B
�?B
�!B
�dB
�qB
�jB
�dB
�9B
�jB
�XB
�^B
�XB
�?B
�?B
�RB
�?B
�FB
�LB
�3B
�-B
�FB
�B
�!B
�!B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
��B
��B
��B
��B
��B
��B
��B
��B
�\B
�bB
��B
��B
�{B
�bB
�DB
�JB
�DB
�+B
|�B
�B
}�B
}�B
� B
�B
{�B
n�B
x�B
}�B
~�B
}�B
z�B
w�B
u�B
u�B
s�B
r�B
n�B
t�B
y�B
y�B
x�B
w�B
u�B
s�B
r�B
u�B
t�B
q�B
n�B
e`B
^5B
^5B
`BB
cTB
]/B
]/B
`BB
_;B
YB
P�B
S�B
P�B
XB
XB
P�B
P�B
J�B
L�B
N�B
P�B
R�B
T�B
S�B
Q�B
P�B
L�B
I�B
H�B
E�B
>wB
?}B
?}B
A�B
A�B
9XB
1'B
C�B
F�B
F�B
E�B
C�B
B�B
C�B
C�B
C�B
A�B
>wB
>wB
=qB
A�B
?}B
=qB
;dB
=qB
<jB
8RB
:^B
9XB
8RB
6FB
1'B
.B
0!B
5?B
8RB
8RB
7LB
49B
1'B
49B
0!B
33B
49B
2-B
2-B
/B
(�B
'�B
&�B
%�B
"�B
.B
0!B
/B
,B
&�B
"�B
"�B
(�B
-B
/B
/B
-B
-B
-B
)�B
&�B
)�B
'�B
$�B
�B
�B
�B
"�B
!�B
�B
�B
hB
�B
�B
�B
 �B
 �B
&�B
&�B
%�B
$�B
"�B
�B
�B
�B
 �B
%�B
$�B
 �B
�B
{B
�B
�B
 �B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
"�B
#�B
!�B
�B
�B
�B
"�B
!�B
 �B
�B
!�B
"�B
#�B
"�B
#�B
"�B
"�B
"�B
$�B
&�B
#�B
�B
$�B
%�B
"�B
"�B
!�B
#�B
%�B
#�B
&�B
&�B
&�B
'�B
(�B
'�B
'�B
(�B
&�B
'�B
%�B
%�B
%�B
(�B
&�B
$�B
)�B
)�B
+B
/B
/B
1'B
0!B
1'B
2-B
0!B
0!B
2-B
5?B
49B
33B
1'B
/B
1'B
33B
33B
49B
33B
33B
49B
49B
33B
0!B
49B
8RB
8RB
7LB
7LB
7LB
6FB
5?B
2-B
9XB
9XB
7LB
6FB
7LB
7LB
;dB
;dB
:^B
;dB
;dB
;dB
;dB
;dB
:^B
;dB
=qB
=qB
<jB
:^B
9XB
=qB
=qB
=qB
=qB
<jB
<jB
;dB
:^B
;dB
;dB
9XB
7LB
49B
5?B
5?B
>wB
>wB
<jB
:^B
;dB
>wB
>wB
=qB
=qB
?}B
=qB
>wB
=qB
>wB
>wB
<jB
:^B
?}B
D�B
C�B
B�B
B�B
A�B
A�B
@�B
B�B
D�B
E�B
H�B
H�B
I�B
J�B
J�B
J�B
I�B
H�B
G�B
E�B
A�B
>wB
A�B
B�B
E�B
G�B
F�B
I�B
H�B
G�B
J�B
K�B
M�B
O�B
N�B
N�B
L�B
L�B
I�B
I�B
G�B
J�B
J�B
K�B
N�B
N�B
Q�B
VB
VB
VB
VB
S�B
P�B
S�B
R�B
R�B
W
B
YB
XB
XB
VB
VB
T�B
S�B
YB
ZB
ZB
YB
W
B
ZB
YB
\)B
]/B
\)B
[#B
XB
ZB
[#B
[#B
[#B
\)B
[#B
\)B
^5B
]/B
\)B
[#B
^5B
\)B
\)B
]/B
aHB
aHB
aHB
bNB
bNB
bNB
aHB
`BB
bNB
aHB
^5B
_;B
_;B
_;B
`BB
bNB
bNB
cTB
e`B
dZB
dZB
cTB
e`B
e`B
ffB
e`B
e`B
dZB
ffB
ffB
e`B
dZB
gmB
gmB
ffB
ffB
hsB
hsB
gmB
e`B
gmB
hsB
hsB
iyB
iyB
hsB
hsB
ffB
e`B
ffB
dZB
hsB
iyB
iyB
iyB
hsB
hsB
jB
jB
jB
jB
k�B
k�B
jB
jB
jB
iyB
jB
jB
l�B
k�B
jB
iyB
hsB
iyB
jB
m�B
m�B
n�B
m�B
m�B
k�B
jB
k�B
m�B
n�B
o�B
n�B
n�B
o�B
o�B
o�B
o�B
n�B
n�B
m�B
l�B
o�B
o�B
m�B
jB
hsB
iyB
n�B
o�B
n�B
m�B
r�B
r�B
r�B
r�B
p�B
q�B
o�B
p�B
q�B
p�B
p�B
p�B
o�B
m�B
n�B
q�B
s�B
s�B
r�B
q�B
r�B
p�B
t�B
s�B
s�B
s�B
s�B
v�B
v�B
v�B
v�B
u�B
v�B
v�B
u�B
v�B
w�B
v�B
t�B
t�B
x�B
{�B
{�B
z�B
z�B
y�B
z�B
y�B
y�B
z�B
|�B
{�B
{�B
y�B
w�B
y�B
z�B
z�B
|�B
}�B
|�B
{�B
x�B
w�B
}�B
|�B
z�B
{�B
~�B
}�B
{�B
z�B
|�B
|�B
~�B
~�B
~�B
}�B
|�B
|�B
~�B
}�B
x�B
y�B
~�B
~�B
|�B
|�B
|�B
~�B
~�B
�B
�B
�B
�%B
�+B
�+B
�%B
�%B
�1B
�+B
�+B
�%B
�+B
�%B
�B
�+B
�1B
�%B
�%B
�+B
�+B
�+B
�+B
�+B
�+B
�+B
�%B
�%B
�+B
�1B
�=B
�=B
�7B
�1B
�1B
�=B
�=B
�7B
�7B
�=B
�DB
�DB
�JB
�PB
�JB
�JB
�PB
�PB
�PB
�VB
�VB
�PB
�VB
�VB
�PB
�VB
�VB
�\B
�\B
�\B
�bB
�hB
�hB
�bB
�\B
�\B
�bB
�hB
�hB
�oB
�oB
�oB
�oB
�oB
�oB
�oB
�oB
�hB
�hB
�uB
�uB
�oB
�uB
�{B
�{B
�{B
�{B
�uB
�uB
�uB
�uB
�oB
�oB
�uB
�uB
�oB
�{B
�{B
��B
��B
��B
��B
��B
�{B
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
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B	C�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.05 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20220416090237                              AO  ARCAADJP                                                                    20220416090237    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20220416090237  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20220416090237  QCF$                G�O�G�O�G�O�4000            