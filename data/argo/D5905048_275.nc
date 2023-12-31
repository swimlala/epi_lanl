CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-08-27T00:35:22Z creation;2018-08-27T00:35:27Z conversion to V3.1;2019-12-19T07:30:16Z update;     
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
_FillValue                 �  I4   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p4   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �4   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �4   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �d   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �t   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �x   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �|   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20180827003522  20200116231517  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL              A   JA  I2_0577_275                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @�|�	�Y�1   @�|�����@4�)^�	�db4m��91   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�33@�  A   A   AA��Aa��A���A���A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B?��BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1fD1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DAy�DB  DB� DC  DC�fDD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D���D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ Dȼ�D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D���D�@ D� D�� D�  D�@ D� D��D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @���@�ff@�ffA33A@��A`��A�ffA�ffA���A���A���Aϙ�Aߙ�AA���B��B��B��B��B'��B/��B7��B?ffBG��BO��BW��B_��Bg��Bo��Bw��B��B��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fC�3C�3C�3C�3C	�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C!�3C#�3C%�3C'�3C)�3C+�3C-�3C/�3C1�3C3�3C5�3C7�3C9�3C;�3C=�3C?�3CA�3CC�3CE�3CG�3CI�3CK�3CM�3CO�3CQ�3CS�3CU�3CW�3CY�3C[�3C]�3C_�3Ca�3Cc�3Ce�3Cg�3Ci�3Ck�3Cm�3Co�3Cq�3Cs�3Cu�3Cw�3Cy�3C{�3C}�3C�3C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D |�D ��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D	|�D	��D
|�D
��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D3D|�D��D|�D��D |�D ��D!|�D!��D"|�D"��D#|�D#��D$|�D$��D%|�D%��D&|�D&��D'|�D'��D(|�D(��D)|�D)��D*|�D*��D+|�D+��D,|�D,��D-|�D-��D.|�D.��D/|�D/��D0|�D13D1|�D1��D2|�D2��D3|�D3��D4|�D4��D5|�D5��D6|�D6��D7|�D7��D8|�D8��D9|�D9��D:|�D:��D;|�D;��D<|�D<��D=|�D=��D>|�D>��D?|�D?��D@|�D@��DAvfDA��DB|�DB��DC�3DC��DD|�DD��DE|�DE��DF|�DF��DG|�DG��DH|�DH��DI|�DI��DJ|�DJ��DK|�DK��DL|�DL��DM|�DM��DN|�DN��DO|�DO��DP|�DP��DQ|�DQ��DR|�DR��DS|�DS��DT|�DT��DU|�DU��DV|�DV��DW|�DW��DX|�DX��DY|�DY��DZ|�DZ��D[|�D[��D\|�D\��D]|�D]��D^|�D^��D_|�D_��D`|�D`��Da|�Da��Db|�Db��Dc|�Dc��Dd|�Dd��De|�De��Df|�Df��Dg|�Dg��Dh|�Dh��Di|�Di��Dj|�Dj��Dk|�Dk��Dl|�Dl��Dm|�Dm��Dn|�Dn��Do|�Do��Dp|�Dp��Dq|�Dq��Dr|�Dr��Ds|�Ds��Dt|�Dt��Du|�Du��Dv|�Dv��Dw|�Dw��Dx|�Dx��Dy|�Dy��Dz|�Dz��D{|�D{��D||�D|��D}|�D}��D~|�D~��D|�D��D�>fD�~fD��fD��fD�>fD�~fD��3D��3D�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��3D�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��D�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD¾fD��fD�>fD�~fDþfD��fD�>fD�~fDľfD��fD�>fD�~fDžfD��fD�>fD�~fDƾfD��fD�>fD�~fDǾfD��fD�>fD�~fDȻ3D��fD�>fD�~fDɾfD��fD�>fD�~fDʾfD��fD�>fD�~fD˾fD��fD�>fD�~fD̾fD��fD�>fD�~fD;fD��fD�>fD�~fDξfD��fD�>fD�~fDϾfD��fD�>fD�~fDоfD��fD�>fD�~fDѾfD��fD�>fD�~fDҾfD��fD�>fD�~fDӾfD��fD�>fD�~fDԾfD��fD�>fD�~fDվfD��fD�>fD�~fD־fD��fD�>fD�~fD׾fD��fD�>fD�~fDؾfD��fD�>fD�~fDپfD��fD�>fD�~fDھfD��fD�>fD�~fD۾fD��fD�>fD�~fDܾfD��fD�>fD�~fDݾfD��fD�>fD�~fD޾fD��fD�>fD�~fD߾fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��3D�>fD�~fD�fD��fD�>fD�~fD�3D��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD��fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��;A��
A��
A��
A���A���A���A���A���A���A���A��
A��
A���A���A��
A��#A��/A��#A���AݶFA�XA���A���A���A�&�A���A�E�A�x�A�
=A���A�l�A��A�JA�"�A��AՏ\A���Aѝ�A��A�dZA�ȴA͟�A�ZA��#A�dZA�
=A˗�A�33A�+Aɺ^A�oAǗ�A�33A�dZAŉ7A��/A��A��A�ȴA��A�A���A�G�A�1A��/A�A���A�r�A��A��A�~�A�VA�33A�A��PA���A��9A��yA�jA��`A�I�A�bA��A�\)A�dZA���A���A�jA�
=A��DA�7LA�x�A��A���A��A�
=A��A���A�7LA�33A�~�A�XA���A�t�A��hA��A��yA�bA�=qA��A��;A�ƨA�hA~��A|1Ay�Av�yAw;dAx�AxffAv��At��AtA�AsS�Aq�-AoC�Aj��AidZAf�uAe�Ac�mAc;dAbJA`bNA^1AZ�AW�AT�DARbAO7LAL�`AJA�AGO�AF��AF �AEl�AD{A@��A@{A>��A:�A7ƨA7XA6E�A5dZA4�A4bNA3�wA2��A2I�A1��A0�A/�
A.=qA+A*jA*-A)�TA(��A&��A%XA$�A#��A!�A ��A��AbA��A^5A��A��A��AoA�uA�A�TA�A�TA�
A"�A�AK�A~�A?}A�9A(�AƨA�7A��AA|�AG�AA�/A�DAffAE�A�A�^Ap�A+AJAz�A�A	hsA	O�AE�AA�#A��A�#A�j@�
=@�j@�@�Z@���@�o@�E�@�Ĝ@�1@��@�7@�P@�^@���@��/@�r�@�\@�G�@�I�@�ƨ@�t�@��@ܓu@���@�o@�p�@؋D@���@�-@��@ղ-@���@�o@��@���@Ο�@�v�@Ϯ@ЋD@��/@��/@Ѓ@ϝ�@�x�@�%@́@�G�@̴9@ʏ\@ț�@�?}@��y@˶F@�9X@ˍP@�n�@�x�@���@�1@�n�@�x�@�(�@�
=@�`B@�Ĝ@�1'@���@�@�@���@���@��w@�o@���@��#@��`@��@�1@��F@���@�C�@��@�^5@���@�O�@�bN@�1@�l�@��@��R@��+@�ff@�^5@�^5@�^5@�^5@�=q@�5?@��@��T@�hs@�X@�?}@�G�@�G�@�O�@���@��;@�l�@�;d@��y@�-@��T@�p�@���@���@�X@���@��@�$�@��!@�;d@��@�"�@�@�&�@�j@�Q�@�|�@�^5@�X@���@�b@�Q�@��F@�C�@�S�@�dZ@��w@�|�@�;d@���@��@�o@��@�~�@�?}@�/@��7@��^@��h@���@�+@�\)@��\@�E�@��@�7L@�Ĝ@�z�@��@�1@�1@�b@�  @��m@��
@��w@���@�33@�o@���@���@�~�@�M�@�J@���@�hs@�`B@�&�@�%@���@�Q�@��@���@�+@�@��y@��H@��!@��+@�~�@�n�@�5?@��@��@���@��^@���@�X@���@��/@��9@��@�9X@���@��;@��w@���@�33@���@�M�@�5?@��@��@�@��-@��@�&�@�j@��@�  @��@��
@�S�@�"�@���@��\@��@���@�`B@�7L@�&�@�%@��@��@�j@��@��;@�|�@�C�@��H@���@�ff@�E�@�J@��#@���@�p�@�G�@�/@��j@�bN@�Z@�9X@��
@��P@�l�@�ȴ@�{@���@��^@�G�@���@��D@�j@�Z@�Q�@�A�@�  @��;@��P@�dZ@�+@���@��@���@�^5@�$�@�J@��T@��h@�G�@��@��@�%@�%@��@���@��@��@�A�@��@��@\)@+@
=@~�@~�R@~v�@~V@~5?@}�@}��@}�@|�@|j@|Z@|1@{t�@{o@z��@zn�@z=q@z�@y��@y&�@x��@x�@xb@w�@w�@w;d@v�@v�+@u��@u`B@t�@t��@tj@t(�@t1@s��@s�F@sC�@r�H@r�!@rM�@q��@q�^@q��@q�7@q�7@q��@q&�@pQ�@p �@o�@o�w@o�P@o|�@o|�@oK�@o;d@o\)@n��@nv�@m�T@m��@mO�@l�D@l�@l(�@k��@kS�@kS�@ko@j�H@j^5@j�@i�@i�^@ihs@h��@h1'@g�@g�@gK�@f��@f5?@f5?@eO�@d�j@d�j@d�@dZ@c�m@cdZ@c33@c"�@co@b��@b~�@b�@a��@aX@`r�@`b@_�w@_l�@_+@^�y@^v�@^V@]�T@]`B@]�@\��@\z�@\9X@[��@[��@["�@Z��@Y�@Yx�@YX@Y�@X�`@X�@X1'@W�@W�P@W;d@V�R@Vff@U��@U�@T�/@TZ@T�@S�m@Sƨ@S�F@S�@St�@So@R^5@Q��@Q��@Qx�@Q%@Q%@Q7L@Q&�@Q�@P��@P��@P�@PQ�@P1'@O�w@O+@N�y@N��@N�@N�R@N�R@N��@NV@N$�@M�h@M�@MV@L��@L�D@Lz�@L(�@K�F@K�@K33@J�H@J�\@J~�@J^5@J�@I��@I�7@IG�@I&�@H�`@H�@H  @G��@G�@F��@F�R@F��@F��@F�R@F��@F5?@E�@E��@E�-@E?}@E`B@E`B@D��@D�D@Dz�@DI�@CdZ@C"�@C"�@B��@B�@BJ@A�@A��@AX@@��@@��@@Q�@?�@?�;@?��@?�P@?|�@?l�@?\)@>�@>��@>�+@>ff@>ff@=�T@=O�@=O�@=�@<�/@<��@<�j@<��@<z�@<Z@<�@;��@;��@;"�@:�@:��@:�\@:J@9�^@9��@97L@8�`@8�9@8b@7�@7l�@7+@6�R@6v�@6E�@6{@5�-@5�@5V@4��@4��@4z�@3�
@3ƨ@3�
@3��@3dZ@3"�@2��@2^5@2M�@2n�@2��@2~�@2=q@2�@1�#@1�^@1��@1�7@1hs@1G�@1�@1%@1�@0��@01'@/�;@/�@/�@/��@/l�@.�@.�R@.V@.{@-p�@,�@,(�@,1@+�
@+t�@*��@*~�@*-@*J@)��@)��@)�7@)X@)%@(�`@(�`@(�`@(Ĝ@(��@(r�@(Q�@(Q�@( �@(b@'�;@'�P@'l�@'\)@'\)@'\)@'K�@'�@&�R@&�+@&E�@&5?@&$�@&$�@&{@%��@%��@%�@%O�@%/@$��@$�@$�@$��@$Z@$�@#�
@#��@#dZ@#C�@#"�@#@"�H@"��@"~�@"J@!��@!x�@!hs@!hs@!&�@!�@ �`@ �u@ �@ r�@   @�w@|�@�y@ff@�T@`B@�@�@��@��@�D@z�@Z@I�@1@��@�m@�m@�m@�
@�
@ƨ@��@��@S�@@��@�!@��@n�@�#@�7@&�@��@Ĝ@Ĝ@�@bN@A�@  @��@|�@\)@;d@+@+@�@�@ȴ@�R@V@��@@�-@�h@?}@V@�@��@��@z�@I�@9X@(�@(�@(�@(�@�@1@�m@ƨ@��@dZ@33@33@33@33@"�@"�@"�@o@o@o@@�H@�!@�\@^5@=q@J@�@�^@X@�@��@Ĝ@�9@�9@��@r�@1'@ �@�@��@�@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��;A��
A��
A��
A���A���A���A���A���A���A���A��
A��
A���A���A��
A��#A��/A��#A���AݶFA�XA���A���A���A�&�A���A�E�A�x�A�
=A���A�l�A��A�JA�"�A��AՏ\A���Aѝ�A��A�dZA�ȴA͟�A�ZA��#A�dZA�
=A˗�A�33A�+Aɺ^A�oAǗ�A�33A�dZAŉ7A��/A��A��A�ȴA��A�A���A�G�A�1A��/A�A���A�r�A��A��A�~�A�VA�33A�A��PA���A��9A��yA�jA��`A�I�A�bA��A�\)A�dZA���A���A�jA�
=A��DA�7LA�x�A��A���A��A�
=A��A���A�7LA�33A�~�A�XA���A�t�A��hA��A��yA�bA�=qA��A��;A�ƨA�hA~��A|1Ay�Av�yAw;dAx�AxffAv��At��AtA�AsS�Aq�-AoC�Aj��AidZAf�uAe�Ac�mAc;dAbJA`bNA^1AZ�AW�AT�DARbAO7LAL�`AJA�AGO�AF��AF �AEl�AD{A@��A@{A>��A:�A7ƨA7XA6E�A5dZA4�A4bNA3�wA2��A2I�A1��A0�A/�
A.=qA+A*jA*-A)�TA(��A&��A%XA$�A#��A!�A ��A��AbA��A^5A��A��A��AoA�uA�A�TA�A�TA�
A"�A�AK�A~�A?}A�9A(�AƨA�7A��AA|�AG�AA�/A�DAffAE�A�A�^Ap�A+AJAz�A�A	hsA	O�AE�AA�#A��A�#A�j@�
=@�j@�@�Z@���@�o@�E�@�Ĝ@�1@��@�7@�P@�^@���@��/@�r�@�\@�G�@�I�@�ƨ@�t�@��@ܓu@���@�o@�p�@؋D@���@�-@��@ղ-@���@�o@��@���@Ο�@�v�@Ϯ@ЋD@��/@��/@Ѓ@ϝ�@�x�@�%@́@�G�@̴9@ʏ\@ț�@�?}@��y@˶F@�9X@ˍP@�n�@�x�@���@�1@�n�@�x�@�(�@�
=@�`B@�Ĝ@�1'@���@�@�@���@���@��w@�o@���@��#@��`@��@�1@��F@���@�C�@��@�^5@���@�O�@�bN@�1@�l�@��@��R@��+@�ff@�^5@�^5@�^5@�^5@�=q@�5?@��@��T@�hs@�X@�?}@�G�@�G�@�O�@���@��;@�l�@�;d@��y@�-@��T@�p�@���@���@�X@���@��@�$�@��!@�;d@��@�"�@�@�&�@�j@�Q�@�|�@�^5@�X@���@�b@�Q�@��F@�C�@�S�@�dZ@��w@�|�@�;d@���@��@�o@��@�~�@�?}@�/@��7@��^@��h@���@�+@�\)@��\@�E�@��@�7L@�Ĝ@�z�@��@�1@�1@�b@�  @��m@��
@��w@���@�33@�o@���@���@�~�@�M�@�J@���@�hs@�`B@�&�@�%@���@�Q�@��@���@�+@�@��y@��H@��!@��+@�~�@�n�@�5?@��@��@���@��^@���@�X@���@��/@��9@��@�9X@���@��;@��w@���@�33@���@�M�@�5?@��@��@�@��-@��@�&�@�j@��@�  @��@��
@�S�@�"�@���@��\@��@���@�`B@�7L@�&�@�%@��@��@�j@��@��;@�|�@�C�@��H@���@�ff@�E�@�J@��#@���@�p�@�G�@�/@��j@�bN@�Z@�9X@��
@��P@�l�@�ȴ@�{@���@��^@�G�@���@��D@�j@�Z@�Q�@�A�@�  @��;@��P@�dZ@�+@���@��@���@�^5@�$�@�J@��T@��h@�G�@��@��@�%@�%@��@���@��@��@�A�@��@��@\)@+@
=@~�@~�R@~v�@~V@~5?@}�@}��@}�@|�@|j@|Z@|1@{t�@{o@z��@zn�@z=q@z�@y��@y&�@x��@x�@xb@w�@w�@w;d@v�@v�+@u��@u`B@t�@t��@tj@t(�@t1@s��@s�F@sC�@r�H@r�!@rM�@q��@q�^@q��@q�7@q�7@q��@q&�@pQ�@p �@o�@o�w@o�P@o|�@o|�@oK�@o;d@o\)@n��@nv�@m�T@m��@mO�@l�D@l�@l(�@k��@kS�@kS�@ko@j�H@j^5@j�@i�@i�^@ihs@h��@h1'@g�@g�@gK�@f��@f5?@f5?@eO�@d�j@d�j@d�@dZ@c�m@cdZ@c33@c"�@co@b��@b~�@b�@a��@aX@`r�@`b@_�w@_l�@_+@^�y@^v�@^V@]�T@]`B@]�@\��@\z�@\9X@[��@[��@["�@Z��@Y�@Yx�@YX@Y�@X�`@X�@X1'@W�@W�P@W;d@V�R@Vff@U��@U�@T�/@TZ@T�@S�m@Sƨ@S�F@S�@St�@So@R^5@Q��@Q��@Qx�@Q%@Q%@Q7L@Q&�@Q�@P��@P��@P�@PQ�@P1'@O�w@O+@N�y@N��@N�@N�R@N�R@N��@NV@N$�@M�h@M�@MV@L��@L�D@Lz�@L(�@K�F@K�@K33@J�H@J�\@J~�@J^5@J�@I��@I�7@IG�@I&�@H�`@H�@H  @G��@G�@F��@F�R@F��@F��@F�R@F��@F5?@E�@E��@E�-@E?}@E`B@E`B@D��@D�D@Dz�@DI�@CdZ@C"�@C"�@B��@B�@BJ@A�@A��@AX@@��@@��@@Q�@?�@?�;@?��@?�P@?|�@?l�@?\)@>�@>��@>�+@>ff@>ff@=�T@=O�@=O�@=�@<�/@<��@<�j@<��@<z�@<Z@<�@;��@;��@;"�@:�@:��@:�\@:J@9�^@9��@97L@8�`@8�9@8b@7�@7l�@7+@6�R@6v�@6E�@6{@5�-@5�@5V@4��@4��@4z�@3�
@3ƨ@3�
@3��@3dZ@3"�@2��@2^5@2M�@2n�@2��@2~�@2=q@2�@1�#@1�^@1��@1�7@1hs@1G�@1�@1%@1�@0��@01'@/�;@/�@/�@/��@/l�@.�@.�R@.V@.{@-p�@,�@,(�@,1@+�
@+t�@*��@*~�@*-@*J@)��@)��@)�7@)X@)%@(�`@(�`@(�`@(Ĝ@(��@(r�@(Q�@(Q�@( �@(b@'�;@'�P@'l�@'\)@'\)@'\)@'K�@'�@&�R@&�+@&E�@&5?@&$�@&$�@&{@%��@%��@%�@%O�@%/@$��@$�@$�@$��@$Z@$�@#�
@#��@#dZ@#C�@#"�@#@"�H@"��@"~�@"J@!��@!x�@!hs@!hs@!&�@!�@ �`@ �u@ �@ r�@   @�w@|�@�y@ff@�T@`B@�@�@��@��@�D@z�@Z@I�@1@��@�m@�m@�m@�
@�
@ƨ@��@��@S�@@��@�!@��@n�@�#@�7@&�@��@Ĝ@Ĝ@�@bN@A�@  @��@|�@\)@;d@+@+@�@�@ȴ@�R@V@��@@�-@�h@?}@V@�@��@��@z�@I�@9X@(�@(�@(�@(�@�@1@�m@ƨ@��@dZ@33@33@33@33@"�@"�@"�@o@o@o@@�H@�!@�\@^5@=q@J@�@�^@X@�@��@Ĝ@�9@�9@��@r�@1'@ �@�@��@�@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B	�3B	�9B	�9B	�9B	�?B	�9B	�?B	�?B	�?B	�?B	�?B	�?B	�?B	�?B	�?B	�?B	�?B	�9B	�-B	�!B	��B	��B	�B	�)B	�B
  B	��B	�B	�5B	�B	��B	B	ÖB	��B	�RB	�LB	�B	�wB	�fB

=B
1'B
?}B
A�B
B�B
]/B
l�B
l�B
u�B
}�B
��B
��B
ƨB
��B
��B
�)B
�B
��B'�B9XBe`Bw�B�=B��B�?B�^B�9B��BɺB�jB��B��B1BDB1BB�B�B�NB�sB�B�B�B�B�#B�-B�?BŢBȴB��B�3B��Bz�B49BF�B`BBS�B-B
��B
�B
��B
�B
�VB
F�B
.B
�B	��B	�wB	�B	��B	�\B	q�B	�B	��B	��B	�bB	|�B	r�B	�B	�^B	��B	B	ƨB	��B	ɺB	�9B	��B	� B	��B	�B	�B	� B	{�B	iyB	T�B	5?B	�B	DB	B��B�B�yB�`B�B�B�B�B�BB��B��B��B��B�dB�?B�9B�9B�3B�B��B��B��B��B��B�DB~�B�+B��B��B�7B�B�+B��B�JB�PB�oB��B�VB��B��B�3B�qB��B��B��B��B��B�#B�#B�
B��BȴB��B��BÖB��B��B�#B�HB�NB�NB��B��B��B	B	B	
=B	DB	JB	JB	hB	oB	+B	B��B��B	B	  B�B�B�HB��B�RB�uBVB�B�+B�=B�+B�%B|�B~�Bv�Bq�BjBk�Bt�B{�Bw�Bo�B{�B� B�%B�%B�Bx�B�B�B�B�B�B�1B�PB�=B�B|�B}�B�JB�\B��B��B�B�B��B��B��B��B��B�?B�?B�B�FB�'BȴB��B�BB�)B�B��BȴB�dB��B�'B�XB�3B�9B�!B�XB�XB�RB�RB�RB�LB�}B�wB��BŢBɺB��B�B�B�;B�NB�HB�`B�yB�B�B�B��B��B	B	JB	hB	{B	�B	�B	�B	�B	�B	�B	 �B	'�B	'�B	-B	-B	.B	/B	1'B	1'B	-B	2-B	5?B	49B	2-B	:^B	;dB	>wB	C�B	L�B	O�B	R�B	W
B	]/B	bNB	dZB	aHB	\)B	e`B	dZB	hsB	cTB	bNB	bNB	iyB	iyB	s�B	x�B	w�B	�B	�B	�1B	�=B	�JB	�bB	��B	��B	��B	��B	�uB	��B	��B	��B	��B	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�3B	�-B	�9B	�?B	�9B	�9B	�9B	�LB	�XB	�RB	�XB	�RB	�LB	�XB	�dB	�jB	��B	ÖB	ĜB	ĜB	ŢB	ǮB	ƨB	ƨB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�
B	�
B	�
B	�
B	�B	��B	��B	�B	�#B	�)B	�)B	�B	�/B	�/B	�#B	�B	�)B	�/B	�;B	�BB	�BB	�BB	�;B	�;B	�;B	�HB	�HB	�NB	�NB	�ZB	�`B	�sB	�mB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	�B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
  B
B
B
B
B
B
%B
+B
+B
+B
%B
B
+B
+B
1B
	7B
1B
1B
1B
	7B
1B

=B

=B
JB
PB
PB
PB
PB
PB
JB
PB
VB
\B
bB
hB
uB
{B
�B
�B
{B
oB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
�B
 �B
!�B
!�B
!�B
!�B
"�B
#�B
"�B
"�B
!�B
 �B
#�B
#�B
"�B
"�B
#�B
$�B
"�B
#�B
'�B
'�B
'�B
&�B
'�B
(�B
)�B
(�B
'�B
(�B
(�B
(�B
)�B
'�B
)�B
)�B
+B
+B
,B
+B
,B
+B
+B
-B
-B
-B
.B
.B
.B
-B
-B
,B
.B
0!B
/B
0!B
/B
/B
0!B
0!B
0!B
/B
0!B
0!B
1'B
0!B
1'B
33B
49B
49B
5?B
49B
49B
33B
2-B
49B
5?B
49B
49B
7LB
9XB
9XB
:^B
:^B
:^B
:^B
:^B
:^B
8RB
9XB
;dB
<jB
=qB
=qB
=qB
<jB
<jB
<jB
;dB
;dB
=qB
<jB
<jB
=qB
<jB
<jB
=qB
=qB
=qB
?}B
@�B
?}B
?}B
>wB
?}B
>wB
@�B
?}B
>wB
>wB
>wB
>wB
=qB
?}B
A�B
B�B
B�B
B�B
A�B
B�B
C�B
B�B
B�B
C�B
D�B
C�B
B�B
C�B
C�B
@�B
C�B
D�B
C�B
C�B
D�B
F�B
E�B
D�B
D�B
E�B
F�B
F�B
H�B
I�B
H�B
I�B
J�B
I�B
H�B
I�B
J�B
J�B
I�B
H�B
G�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
I�B
J�B
I�B
I�B
I�B
J�B
I�B
H�B
I�B
J�B
I�B
J�B
J�B
I�B
I�B
K�B
K�B
K�B
L�B
L�B
M�B
L�B
M�B
M�B
M�B
O�B
N�B
M�B
P�B
Q�B
P�B
P�B
P�B
P�B
P�B
R�B
S�B
T�B
T�B
S�B
T�B
S�B
T�B
VB
VB
VB
VB
VB
VB
VB
T�B
R�B
T�B
VB
W
B
W
B
VB
T�B
W
B
W
B
W
B
VB
VB
W
B
YB
YB
XB
XB
YB
ZB
[#B
[#B
[#B
[#B
[#B
[#B
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
_;B
`BB
`BB
`BB
_;B
_;B
^5B
_;B
`BB
aHB
aHB
aHB
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
_;B
aHB
aHB
aHB
aHB
bNB
cTB
cTB
cTB
bNB
bNB
bNB
bNB
dZB
dZB
dZB
dZB
dZB
dZB
cTB
e`B
dZB
cTB
cTB
dZB
cTB
cTB
e`B
e`B
gmB
gmB
iyB
jB
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
iyB
iyB
iyB
k�B
jB
iyB
hsB
jB
jB
k�B
m�B
l�B
l�B
l�B
m�B
l�B
l�B
m�B
m�B
n�B
n�B
n�B
n�B
m�B
n�B
m�B
l�B
l�B
o�B
o�B
o�B
n�B
o�B
o�B
p�B
o�B
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
p�B
q�B
p�B
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
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
q�B
r�B
s�B
t�B
t�B
t�B
t�B
s�B
s�B
t�B
t�B
t�B
u�B
v�B
v�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B	�MB	�9B	�TB	�TB	�?B	�TB	�?B	�?B	�?B	�?B	�?B	�?B	�ZB	�?B	�?B	�?B	�?B	�TB	�aB	��B	��B	�KB	�;B	��B	�B
 �B	�XB	��B	�!B	֡B	˒B	�aB	�gB	ðB	�0B	��B	�3B	�B	��B
�B
2-B
?�B
BAB
C�B
^5B
mCB
m�B
v�B
�B
�B
��B
�_B
�B
өB
�B
��B
�xB($B:^Bf�By	B��B��B��B��B�B�~B�^B��B� B�tB�B�B	B�B�UB�iB�fB�B�OB�#B�vB��B��B�B��B��B��BB��B��B�B<PBJ�Ba�BV9B0;B
��B
�dB
��B
��B
�@B
LdB
0�B
!-B	��B	��B	�oB	��B	��B	v�B	��B	��B	�ZB	��B	�B	t�B	�IB	��B	��B	��B	ȀB	��B	�)B	��B	�_B	��B	�sB	�MB	��B	�oB	|�B	kQB	WsB	8�B	!HB	�B	�B�.B�5B�B�B�WB�B�B��B�]B�B�vB�B��B��B�6B��B�ZB�B�B�;B�KB��B��B�B�B��B��B��B�B�_B�DB�mB��B�kB�pB�vB�,B��B��B�
B��B�B��B�BϑBϫB�}B�gB�#B�WB׍B��B�#B��B�B�B҉BҽB��B��B�TB��B�*B�BB�]B	aB	�B	
�B	�B	�B	�B	�B	@B	�B	9B	B��B	�B	oB�|B�B�BοB��B�B\xB��B�B��B��B��B}�B�Bw�Br�Bk�Bl�Bu?B|6BxlBp�B|�B��B��B��B��BzxB��B��B�AB��B�B��B��B��B��B~]B}B�B�B��B�8B��B�B�0B�fB��B��B�2B�B��B��B��B�aB�1B�,B��B�)B֡BοB��B��B��B�GB�*B�9B�?B�[B��B��B��B��B�	B�B��B�.B� B�B�XB�~B�1BٚB�pB�B�B�B��B�B�-B�aB�$B�dB	uB	~B	�B	�B	�B	�B	�B	�B	�B	�B	!B	($B	(XB	-)B	-)B	.B	/5B	1[B	1�B	-�B	2�B	5�B	4�B	2�B	:�B	;�B	>�B	C�B	L~B	O�B	R�B	V�B	\�B	bB	dZB	a�B	]B	e�B	d�B	h�B	dB	c B	c B	i�B	i�B	s�B	y>B	x8B	�B	�B	�1B	�rB	��B	��B	��B	��B	��B	�
B	�,B	��B	��B	��B	�B	��B	��B	��B	�B	�B	��B	�#B	��B	��B	�B	��B	�B	��B	�B	�B	�B	�CB	�cB	��B	�MB	�|B	�TB	�tB	�nB	�nB	��B	��B	�rB	��B	�rB	��B	��B	��B	��B	��B	��B	ðB	��B	��B	żB	��B	��B	��B	��B	�	B	��B	��B	��B	��B	��B	�B	��B	�B	�(B	�B	�B	� B	� B	�.B	�4B	�@B	�$B	�$B	�YB	�$B	�$B	�9B	�MB	�{B	�EB	�=B	�)B	�]B	ڠB	�IB	�dB	�qB	�B	�xB	�dB	�pB	�vB	�\B	�\B	�pB	�pB	ߊB	�|B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	�B	��B	��B	��B	�B	��B	� B	��B	��B	�5B	�B	��B	��B	� B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�	B	�$B	�	B	�*B	��B	�B	�	B	�B	�0B	�B	�"B	��B	�(B	�B	�(B	�(B	�<B	�<B
 B
 B
 B
'B
'B
'B
'B
GB
-B
AB
'B
'B
 OB
'B
-B
AB
[B
gB
YB
EB
EB
EB
YB
SB
EB
_B
fB
	RB
fB
fB
�B
	lB
�B

�B

rB
~B
�B
�B
�B
�B
jB
~B
jB
pB
�B
}B
�B
�B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
�B
 �B
!�B
!�B
!�B
!�B
"�B
$B
"�B
#B
!�B
!B
#�B
#�B
#B
# B
$B
$�B
# B
$B
'�B
($B
(
B
'B
($B
)*B
*B
)B
($B
)B
)*B
)*B
*0B
(XB
*B
*B
+6B
+B
,=B
+6B
,=B
+6B
+QB
-)B
-CB
-)B
.IB
./B
.IB
-CB
-CB
,WB
.IB
0;B
/5B
0;B
/OB
/OB
0;B
0oB
0;B
/OB
0oB
0UB
1[B
0oB
1[B
3MB
4nB
4TB
5?B
4TB
4TB
3hB
2�B
4TB
5ZB
4TB
4nB
7fB
9XB
9rB
:xB
:xB
:xB
:xB
:xB
:xB
8�B
9�B
;B
<jB
=�B
=�B
=qB
<�B
<�B
<�B
;�B
;�B
=qB
<�B
<�B
=�B
<�B
<�B
=�B
=�B
=�B
?�B
@�B
?�B
?�B
>�B
?�B
>�B
@�B
?�B
>�B
>�B
>�B
>�B
=�B
?�B
A�B
B�B
B�B
B�B
A�B
B�B
C�B
B�B
B�B
C�B
D�B
C�B
B�B
C�B
C�B
@�B
C�B
D�B
C�B
C�B
D�B
F�B
E�B
D�B
D�B
E�B
F�B
F�B
H�B
I�B
H�B
I�B
J�B
I�B
H�B
I�B
J�B
J�B
I�B
H�B
G�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
I�B
J�B
I�B
I�B
I�B
J�B
I�B
IB
I�B
J�B
I�B
J�B
J�B
I�B
I�B
K�B
K�B
LB
L�B
L�B
M�B
MB
M�B
NB
M�B
O�B
OB
NB
P�B
Q�B
Q B
Q B
Q B
Q B
QB
R�B
S�B
T�B
U2B
TB
U2B
TB
UB
VB
VB
V9B
VB
VB
VB
VB
UB
S&B
UB
VB
W
B
W$B
V9B
U2B
W$B
W?B
WYB
VmB
VSB
WYB
Y1B
Y1B
XEB
X_B
YKB
Z7B
[=B
[WB
[=B
[=B
[=B
[=B
\CB
]/B
]/B
]IB
]IB
]IB
^OB
^5B
^OB
^OB
^OB
^OB
_VB
`BB
`BB
`\B
_pB
_VB
^�B
_pB
`\B
aHB
aHB
aHB
`\B
`vB
`\B
`vB
a|B
a|B
abB
aHB
abB
abB
_pB
a|B
a|B
abB
abB
bhB
cnB
cnB
c�B
bhB
b�B
b�B
b�B
dtB
dZB
dZB
d�B
dtB
dtB
cnB
e`B
dtB
c�B
cnB
d�B
c�B
c�B
e�B
e�B
g�B
g�B
i�B
jB
i�B
iyB
i�B
i�B
i�B
jB
j�B
jB
jB
jB
j�B
jB
j�B
j�B
i�B
i�B
i�B
k�B
j�B
i�B
h�B
j�B
j�B
k�B
m�B
l�B
l�B
l�B
m�B
l�B
l�B
m�B
m�B
n�B
n�B
n�B
n�B
m�B
n�B
m�B
l�B
l�B
o�B
o�B
o�B
n�B
o�B
o�B
p�B
o�B
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
p�B
q�B
p�B
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
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
q�B
r�B
s�B
t�B
t�B
t�B
t�B
s�B
s�B
t�B
t�B
t�B
u�B
v�B
v�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.05(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201808310041482018083100414820180831004148201808310200252018083102002520180831020025201809010031112018090100311120180901003111  JA  ARFMdecpA19c                                                                20180827093512  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180827003522  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180827003525  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180827003526  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180827003526  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180827003526  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180827003526  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180827003526  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180827003527  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180827003527                      G�O�G�O�G�O�                JA  ARUP                                                                        20180827005607                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180827153846  CV  JULD            G�O�G�O�F��                JM  ARCAJMQC2.0                                                                 20180830154148  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180830154148  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180830170025  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180831153111  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116231517                      G�O�G�O�G�O�                