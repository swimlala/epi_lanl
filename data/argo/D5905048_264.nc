CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-07-25T00:35:27Z creation;2018-07-25T00:35:31Z conversion to V3.1;2019-12-19T07:32:49Z update;     
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
_FillValue                 �  I$   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `t   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �h   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �L   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �@   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �$   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �4   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ܼ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �L   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �L   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �L   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �L   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �,   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �0   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �@   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �D   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �H   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �LArgo profile    3.1 1.2 19500101000000  20180725003527  20200116231516  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL              A   JA  I2_0577_264                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @�tvF�Z�1   @�tw-�� @3��!-w�d\��,<�1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @33@�  @�33A   A   A@  A`  A�  A�  A�  A�  A�  Aљ�A�ffA�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bg��Bp  Bx  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dyy�Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ D҃3D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�|�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @  @|��@���@�ffA33A?33A_33A33A���A���A���A���A�33A�  AA���B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��BgffBo��Bw��B��B��fB��fB��B��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��3B��fC�3C�3C�3C�3C	�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C!�3C#�3C%�3C'�3C)�3C+�3C-�3C/�3C1�3C3�3C5�3C7�3C9�3C;�3C=�3C?�3CA�3CC�3CE�3CG�3CI�3CK�3CM�3CO�3CQ�3CS�3CU�3CW�3CY�3C[�3C]�3C_�3Ca�3Cc�3Ce�3Cg�3Ci�3Ck�3Cm�3Co�3Cq�3Cs�3Cu�3Cw�3Cy�3C{�3C}�3C�3C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D |�D ��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D	|�D	��D
|�D
��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D3D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D |�D ��D!|�D!��D"|�D"��D#|�D#��D$|�D$��D%|�D%��D&|�D&��D'|�D'��D(|�D(��D)|�D)��D*|�D*��D+|�D+��D,|�D,��D-|�D-��D.|�D.��D/|�D/��D0|�D0��D1|�D1��D2|�D2��D3|�D3��D4|�D4��D5|�D5��D6|�D6��D7|�D7��D8|�D8��D9|�D9��D:|�D:��D;|�D;��D<|�D<��D=|�D=��D>|�D>��D?|�D?��D@|�D@��DA|�DA��DB|�DB��DC|�DC��DD|�DD��DE|�DE��DF|�DF��DG|�DG��DH|�DH��DI|�DI��DJ|�DJ��DK|�DK��DL|�DL��DM|�DM��DN|�DN��DO|�DO��DP|�DP��DQ|�DQ��DR|�DR��DS|�DS��DT|�DT��DU|�DU��DV|�DV��DW|�DW��DX|�DX��DY|�DY��DZ|�DZ��D[|�D[��D\|�D\��D]|�D]��D^|�D^��D_|�D_��D`|�D`��Da|�Da��Db|�Db��Dc|�Dc��Dd|�Dd��De|�De��Df|�Df��Dg|�Dg��Dh|�Dh��Di|�Di��Dj|�Dj��Dk|�Dk��Dl|�Dl��Dm|�Dm��Dn|�Dn��Do|�Do��Dp|�Dp��Dq|�Dq��Dr|�Dr��Ds|�Ds��Dt|�Dt��Du|�Du��Dv|�Dv��Dw|�Dw��Dx|�Dx��DyvfDy��Dz|�Dz��D{|�D{��D||�D|��D}|�D}��D~|�D~��D|�D��D�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�{3D��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�{3D��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��D�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�;3D�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�A�D�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD¾fD��fD�>fD�~fDþfD��fD�>fD�~fDľfD��fD�>fD�~fDžfD��fD�>fD�~fDƾfD��fD�>fD�~fDǾfD��fD�>fD�~fDȾfD��fD�>fD�~fDɾfD��fD�>fD�~fDʾfD��fD�>fD�~fD˾fD��fD�>fD�~fD̾fD��fD�>fD�~fD;fD��fD�>fD�~fDξfD��fD�>fD�~fDϾfD��fD�>fD�~fDоfD��fD�>fD�~fDѾfD��fD�>fDҁ�DҾfD��fD�>fD�~fDӾfD��fD�>fD�~fDԾfD��fD�>fD�~fDվfD��fD�>fD�~fD־fD��fD�>fD�~fD׾fD��fD�>fD�~fDؾfD��fD�>fD�~fDپfD��fD�>fD�~fDھfD��fD�>fD�~fD۾fD��fD�>fD�~fDܾfD��fD�>fD�~fDݾfD��fD�>fD�~fD޾fD��fD�>fD�~fD߾fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD��fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�A�D�{31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�z�A�t�A�VA�9XA�-A� �A�oA�VA�%A�A���A��A��#Aۗ�A�v�A�Q�Aџ�A�ffA���A��yA���Aò-A�|�A�{A��A�oA��#A���A���A�C�A�S�A���A�E�A���A�/A��A�A��+A��A��^A���A��7A�l�A�5?A��A�C�A��A��A�ƨA�~�A���A�K�A�&�A���A���A�I�A�A�;dA��A�{A���A�$�A�l�A��;A��A��-A��;A��hA��mA�ĜA��jA�E�A��+A��A��RA���A�O�A���A�5?A��A�$�A�t�A��A���A���A��-A�{A�v�A���A�^5A�%A�bNA���A���A�z�A���A�ffA��^A�%A�z�A�{A��!A�|�A�
=A�jA�1'A���A��A�z�AG�A~��A~�jA~ZA}�A{x�Ay��AxM�Awx�Av$�At��AtbNAtAqAo�Amx�Aj=qAh�`Ag��Ag%AfA�Ae��Ae��Ae\)Ae33Ae�AdE�Ab�Aa/A^��A]�;A\~�A[�#AZ��AY"�AX  AV�jAT��AR~�AQ��AP��AOG�AN5?ALffAJM�AH �AE�
AC�mABffAA��A@�A=��A;�A7�;A4�RA3�mA1XA0JA-��A*5?A)/A(r�A&��A%�A$��A#x�A"$�A!l�A �RA�HA/A��A$�A�jA�A1'A�#A��A?}AE�A�A��A-A�7AVA��A �A;dA9XA33A
ȴA	�wA�
A�jA��AoA�9A�Ax�AVAJAdZA ��A n�@��P@�Z@���@���@�hs@��@��;@��@�$�@��@���@�"�@�$�@�&�@�u@�C�@�v�@�5?@�hs@�@�t�@�!@��@���@�bN@�1'@�S�@��@��`@�I�@��T@��@�K�@��@���@���@�@��/@���@�^5@ա�@�p�@���@�z�@��m@�C�@�=q@�@�t�@ʇ+@�^5@�$�@ɲ-@�G�@��@�j@�  @ǶF@�t�@�+@��@Ɨ�@��@�X@�b@���@§�@�G�@���@���@��y@��@�p�@�X@�7L@�%@���@�\)@�"�@��@�5?@���@��@���@��@�n�@��7@���@�j@��m@�S�@�
=@��+@��^@��/@��u@�j@�j@��@���@�@��-@�V@� �@��w@��P@�@�ȴ@��\@�^5@�V@�@�&�@���@��@�1'@� �@��m@�ƨ@���@�ȴ@�J@��^@�/@��/@��9@�Ĝ@���@��/@�%@���@���@� �@��
@���@��@���@��@�dZ@�S�@�K�@��@��\@�v�@�M�@�=q@��@���@�hs@�G�@�`B@�?}@�7L@��@��`@��u@��u@�r�@�9X@�  @�ƨ@��!@��T@��^@��#@���@���@��@���@���@��@��@�Ĝ@�(�@���@�C�@�+@�@��@��@���@��h@�`B@��@��@�9X@�1'@�9X@��@��w@�t�@�C�@�;d@�o@��y@��H@��!@���@�^5@�$�@���@���@���@�7L@���@���@�Z@�(�@���@�;d@���@�~�@�^5@�=q@��@��-@�/@��j@��9@�z�@���@���@�K�@�"�@���@��R@�V@��\@��+@�$�@��T@��h@��@��/@���@��D@���@��P@�l�@�S�@�;d@��@��+@�n�@�V@�$�@�@�`B@��@���@��j@�z�@�Q�@���@��w@��@�t�@�C�@���@��+@�~�@�ff@�M�@�{@��@���@��^@���@��@�?}@��@�%@��@��j@���@��u@�bN@�1'@� �@��@��@��;@��@�|�@�C�@�"�@��@��y@���@�ȴ@�ȴ@���@�^5@�-@���@��T@��h@�p�@�/@���@���@���@��j@�j@��@�@��@�w@�P@;d@~ȴ@~@}��@}�T@~{@}@}�@|��@}`B@}�-@~@~E�@~��@~ff@}@}��@|�@|Z@{ƨ@{S�@{o@z�@z�!@zn�@z=q@y�@y��@y��@yG�@x�`@x1'@w|�@v��@vV@v$�@u@u�h@u�@t�@t(�@s�
@sƨ@sƨ@s�F@s33@s@r^5@r-@q��@pbN@o�;@o��@ol�@o
=@nȴ@n�R@n��@n@m�-@m`B@mV@l�/@l�D@lj@l9X@l(�@kƨ@k��@j�H@j��@j~�@jJ@i��@i�7@i�@hĜ@h�9@h��@h�@h1'@g�w@g\)@f��@fv�@f{@e�T@e@e`B@d��@d�D@d(�@c��@c��@b��@bM�@aG�@`Ĝ@`r�@`1'@_��@_K�@_+@^ȴ@^�+@^V@^{@]�@]@]��@]�@]O�@]?}@\��@\j@[��@Z~�@Z-@Y�#@Y��@YX@Xr�@X  @W��@WK�@V�y@V�@Vv�@V5?@U�-@Up�@U?}@T�@T��@TI�@S�
@S��@St�@S"�@R�\@Q�#@Q�7@QG�@Q&�@Q�@Q�@Q&�@Q�@P��@P��@P�u@PA�@O�;@O�w@O�P@O
=@N�R@Nff@N$�@M��@M�@L�j@L��@L9X@K�F@K��@KS�@Ko@J�@J��@J��@Jn�@J�@IG�@H�@Hr�@G�@G�@G�@G�w@G�w@G�@G\)@G;d@F��@Fȴ@Fv�@F$�@F@F@E�T@E�-@E��@E�h@E?}@D�@Dj@C��@Cƨ@C��@CdZ@CC�@Co@B��@B�!@B=q@B�@A�@A��@AG�@A%@@�u@@1'@@b@?�@?|�@?\)@>��@>��@>ff@>V@>$�@>@=��@=?}@<�/@<��@<1@;ƨ@;�@;@:��@:�\@:~�@9�#@9hs@8��@8��@8�@8A�@7�@7;d@6��@5�@4Z@3ƨ@3��@3��@3t�@2��@2�!@2�\@2�@1��@1X@0��@0��@0bN@0b@/��@/K�@/�@.��@.�R@.v�@.v�@.ff@.ff@.$�@-�@-�-@-�@-`B@-?}@,��@,�@,I�@,1@+��@+�@+dZ@*��@*=q@*J@)��@)G�@(��@(bN@(  @'�;@'|�@'+@&�y@&��@&�+@&ff@&V@&$�@%��@%��@%`B@%V@$�@$�/@$��@$�D@$z�@$Z@$1@#�F@#�@#dZ@#S�@#"�@"�H@"��@"��@"��@"��@"~�@"^5@"=q@"-@"J@!��@!�7@ ��@ �@ 1'@  �@ b@   @   @�@��@�w@�P@|�@;d@�@�@��@v�@@��@p�@�/@�@�@Z@�@��@�
@�F@�F@�@dZ@S�@C�@"�@@~�@M�@�@�#@x�@��@��@��@�9@��@�@bN@  @��@��@\)@;d@�@
=@�y@ȴ@�+@5?@��@�h@�@/@V@��@�@��@j@Z@(�@�
@�@@n�@M�@�@��@�#@��@X@�`@��@bN@Q�@ �@�;@�w@��@l�@;d@��@�y@�@�R@�+@{@�T@�-@�h@p�@/@��@�/@�@�D@z�@j@j@9X@1@��@�
@�F@�@dZ@C�@C�@o@
�@
��@
�\@
n�@
^5@
M�@
�@	�^@	X@	7L@	�@��@��@��@r�@Q�@  @��@��@�P@|�@l�@\)@\)@;d@
=@��@�y@�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�z�A�t�A�VA�9XA�-A� �A�oA�VA�%A�A���A��A��#Aۗ�A�v�A�Q�Aџ�A�ffA���A��yA���Aò-A�|�A�{A��A�oA��#A���A���A�C�A�S�A���A�E�A���A�/A��A�A��+A��A��^A���A��7A�l�A�5?A��A�C�A��A��A�ƨA�~�A���A�K�A�&�A���A���A�I�A�A�;dA��A�{A���A�$�A�l�A��;A��A��-A��;A��hA��mA�ĜA��jA�E�A��+A��A��RA���A�O�A���A�5?A��A�$�A�t�A��A���A���A��-A�{A�v�A���A�^5A�%A�bNA���A���A�z�A���A�ffA��^A�%A�z�A�{A��!A�|�A�
=A�jA�1'A���A��A�z�AG�A~��A~�jA~ZA}�A{x�Ay��AxM�Awx�Av$�At��AtbNAtAqAo�Amx�Aj=qAh�`Ag��Ag%AfA�Ae��Ae��Ae\)Ae33Ae�AdE�Ab�Aa/A^��A]�;A\~�A[�#AZ��AY"�AX  AV�jAT��AR~�AQ��AP��AOG�AN5?ALffAJM�AH �AE�
AC�mABffAA��A@�A=��A;�A7�;A4�RA3�mA1XA0JA-��A*5?A)/A(r�A&��A%�A$��A#x�A"$�A!l�A �RA�HA/A��A$�A�jA�A1'A�#A��A?}AE�A�A��A-A�7AVA��A �A;dA9XA33A
ȴA	�wA�
A�jA��AoA�9A�Ax�AVAJAdZA ��A n�@��P@�Z@���@���@�hs@��@��;@��@�$�@��@���@�"�@�$�@�&�@�u@�C�@�v�@�5?@�hs@�@�t�@�!@��@���@�bN@�1'@�S�@��@��`@�I�@��T@��@�K�@��@���@���@�@��/@���@�^5@ա�@�p�@���@�z�@��m@�C�@�=q@�@�t�@ʇ+@�^5@�$�@ɲ-@�G�@��@�j@�  @ǶF@�t�@�+@��@Ɨ�@��@�X@�b@���@§�@�G�@���@���@��y@��@�p�@�X@�7L@�%@���@�\)@�"�@��@�5?@���@��@���@��@�n�@��7@���@�j@��m@�S�@�
=@��+@��^@��/@��u@�j@�j@��@���@�@��-@�V@� �@��w@��P@�@�ȴ@��\@�^5@�V@�@�&�@���@��@�1'@� �@��m@�ƨ@���@�ȴ@�J@��^@�/@��/@��9@�Ĝ@���@��/@�%@���@���@� �@��
@���@��@���@��@�dZ@�S�@�K�@��@��\@�v�@�M�@�=q@��@���@�hs@�G�@�`B@�?}@�7L@��@��`@��u@��u@�r�@�9X@�  @�ƨ@��!@��T@��^@��#@���@���@��@���@���@��@��@�Ĝ@�(�@���@�C�@�+@�@��@��@���@��h@�`B@��@��@�9X@�1'@�9X@��@��w@�t�@�C�@�;d@�o@��y@��H@��!@���@�^5@�$�@���@���@���@�7L@���@���@�Z@�(�@���@�;d@���@�~�@�^5@�=q@��@��-@�/@��j@��9@�z�@���@���@�K�@�"�@���@��R@�V@��\@��+@�$�@��T@��h@��@��/@���@��D@���@��P@�l�@�S�@�;d@��@��+@�n�@�V@�$�@�@�`B@��@���@��j@�z�@�Q�@���@��w@��@�t�@�C�@���@��+@�~�@�ff@�M�@�{@��@���@��^@���@��@�?}@��@�%@��@��j@���@��u@�bN@�1'@� �@��@��@��;@��@�|�@�C�@�"�@��@��y@���@�ȴ@�ȴ@���@�^5@�-@���@��T@��h@�p�@�/@���@���@���@��j@�j@��@�@��@�w@�P@;d@~ȴ@~@}��@}�T@~{@}@}�@|��@}`B@}�-@~@~E�@~��@~ff@}@}��@|�@|Z@{ƨ@{S�@{o@z�@z�!@zn�@z=q@y�@y��@y��@yG�@x�`@x1'@w|�@v��@vV@v$�@u@u�h@u�@t�@t(�@s�
@sƨ@sƨ@s�F@s33@s@r^5@r-@q��@pbN@o�;@o��@ol�@o
=@nȴ@n�R@n��@n@m�-@m`B@mV@l�/@l�D@lj@l9X@l(�@kƨ@k��@j�H@j��@j~�@jJ@i��@i�7@i�@hĜ@h�9@h��@h�@h1'@g�w@g\)@f��@fv�@f{@e�T@e@e`B@d��@d�D@d(�@c��@c��@b��@bM�@aG�@`Ĝ@`r�@`1'@_��@_K�@_+@^ȴ@^�+@^V@^{@]�@]@]��@]�@]O�@]?}@\��@\j@[��@Z~�@Z-@Y�#@Y��@YX@Xr�@X  @W��@WK�@V�y@V�@Vv�@V5?@U�-@Up�@U?}@T�@T��@TI�@S�
@S��@St�@S"�@R�\@Q�#@Q�7@QG�@Q&�@Q�@Q�@Q&�@Q�@P��@P��@P�u@PA�@O�;@O�w@O�P@O
=@N�R@Nff@N$�@M��@M�@L�j@L��@L9X@K�F@K��@KS�@Ko@J�@J��@J��@Jn�@J�@IG�@H�@Hr�@G�@G�@G�@G�w@G�w@G�@G\)@G;d@F��@Fȴ@Fv�@F$�@F@F@E�T@E�-@E��@E�h@E?}@D�@Dj@C��@Cƨ@C��@CdZ@CC�@Co@B��@B�!@B=q@B�@A�@A��@AG�@A%@@�u@@1'@@b@?�@?|�@?\)@>��@>��@>ff@>V@>$�@>@=��@=?}@<�/@<��@<1@;ƨ@;�@;@:��@:�\@:~�@9�#@9hs@8��@8��@8�@8A�@7�@7;d@6��@5�@4Z@3ƨ@3��@3��@3t�@2��@2�!@2�\@2�@1��@1X@0��@0��@0bN@0b@/��@/K�@/�@.��@.�R@.v�@.v�@.ff@.ff@.$�@-�@-�-@-�@-`B@-?}@,��@,�@,I�@,1@+��@+�@+dZ@*��@*=q@*J@)��@)G�@(��@(bN@(  @'�;@'|�@'+@&�y@&��@&�+@&ff@&V@&$�@%��@%��@%`B@%V@$�@$�/@$��@$�D@$z�@$Z@$1@#�F@#�@#dZ@#S�@#"�@"�H@"��@"��@"��@"��@"~�@"^5@"=q@"-@"J@!��@!�7@ ��@ �@ 1'@  �@ b@   @   @�@��@�w@�P@|�@;d@�@�@��@v�@@��@p�@�/@�@�@Z@�@��@�
@�F@�F@�@dZ@S�@C�@"�@@~�@M�@�@�#@x�@��@��@��@�9@��@�@bN@  @��@��@\)@;d@�@
=@�y@ȴ@�+@5?@��@�h@�@/@V@��@�@��@j@Z@(�@�
@�@@n�@M�@�@��@�#@��@X@�`@��@bN@Q�@ �@�;@�w@��@l�@;d@��@�y@�@�R@�+@{@�T@�-@�h@p�@/@��@�/@�@�D@z�@j@j@9X@1@��@�
@�F@�@dZ@C�@C�@o@
�@
��@
�\@
n�@
^5@
M�@
�@	�^@	X@	7L@	�@��@��@��@r�@Q�@  @��@��@�P@|�@l�@\)@\)@;d@
=@��@�y@�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  BPBJBJBPBPBPBVBPB\B\B\B\BVB  B�B2-Bk�B�bB��B��BɺB��B"�BE�B-BI�BffBW
BP�Bn�B�B�+B�%B�hB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B��B��B�oB�=Bx�Bm�BG�BJ�BVBE�B,B/B'�B�B�B�jB��B�^B��B��B}�B�1B�B�+B�+B|�BiyBjBbNBZBP�BO�BO�BD�B+B(�B!�B{B�BoB
��B
��B
�B
�B
�TB
�B
��B
ÖB
�dB
�RB
��B
�VB
{�B
�%B
�PB
�B
q�B
p�B
jB
m�B
p�B
gmB
XB
=qB
8RB
7LB
7LB
-B
�B
)�B
�B	��B	�B	�B	ƨB	��B	�
B	��B	��B	��B	��B	��B	ȴB	��B	�'B	��B	�oB	� B	�B	z�B	y�B	m�B	\)B	S�B	K�B	;dB	%�B	:^B	-B	�B	�B	1B�B�TB�B��B��B�BƨB�B��B�=B�B��B�%B�Bt�B[#Bt�B|�Bl�Bz�Bt�Bp�Bp�Bt�Bo�B^5BW
BXBR�BT�BN�B\)BffBcTB\)BM�B;dBF�BVBT�BS�BQ�BN�BD�B?}B>wBA�B33B'�B49B9XBF�BF�BB�B@�B9XBN�BJ�BJ�BVBK�B?}BO�B[#B^5BZB\)B]/B]/B]/Be`BgmBffBiyBm�Bk�Bq�Bw�Bu�Bs�Bv�By�By�B}�B�B�B~�B� Bw�B}�Bu�By�Bt�Bv�Bx�Bp�B|�B|�B� B}�B�7B�PB�DB�JB�=B�%B� Bm�B}�B�hB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�-B�dB��B��B�wB�^B��BƨBǮBĜBǮB��B��BȴBȴB�
B�;B�fB�mB�yB�B�B�B��B	B	B	B��B	B	DB	hB	\B	bB	�B	�B	 �B	!�B	$�B	&�B	(�B	(�B	'�B	1'B	6FB	49B	8RB	7LB	9XB	8RB	6FB	8RB	A�B	A�B	G�B	M�B	S�B	T�B	T�B	VB	W
B	VB	VB	XB	`BB	ffB	e`B	gmB	l�B	n�B	o�B	m�B	q�B	z�B	{�B	� B	� B	}�B	�B	�B	�%B	�%B	�%B	�%B	�1B	�7B	�VB	�PB	�PB	�JB	�DB	�B	�1B	�bB	��B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�'B	�9B	�?B	�?B	�FB	�XB	�jB	�wB	�qB	�}B	��B	B	ÖB	ÖB	ÖB	ŢB	ƨB	ƨB	ŢB	ƨB	��B	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�
B	�B	�/B	�5B	�#B	�/B	�)B	�)B	�5B	�BB	�/B	�B	�5B	�BB	�HB	�HB	�NB	�NB	�`B	�fB	�`B	�`B	�fB	�sB	�B	�B	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B	��B	��B	��B	��B	��B	��B	��B
  B	��B
  B
B
B
B	��B
  B
B
B
B
B
B
B
B
B
1B
	7B
+B
%B
1B
VB
\B
bB
hB
{B
uB
hB
uB
hB
oB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
"�B
!�B
!�B
!�B
#�B
$�B
#�B
$�B
$�B
$�B
%�B
'�B
&�B
&�B
&�B
$�B
%�B
%�B
$�B
%�B
&�B
&�B
%�B
%�B
%�B
&�B
'�B
&�B
%�B
&�B
&�B
(�B
)�B
+B
,B
+B
-B
-B
.B
/B
/B
0!B
0!B
1'B
1'B
1'B
1'B
/B
.B
.B
.B
2-B
2-B
2-B
2-B
0!B
33B
5?B
5?B
6FB
7LB
6FB
6FB
6FB
7LB
8RB
7LB
7LB
8RB
8RB
8RB
9XB
8RB
7LB
7LB
:^B
;dB
;dB
<jB
<jB
<jB
;dB
;dB
:^B
:^B
9XB
9XB
;dB
:^B
9XB
:^B
:^B
:^B
:^B
9XB
;dB
=qB
<jB
<jB
>wB
>wB
>wB
?}B
?}B
>wB
=qB
<jB
;dB
=qB
A�B
@�B
A�B
C�B
C�B
C�B
B�B
A�B
B�B
B�B
C�B
B�B
C�B
D�B
D�B
D�B
D�B
D�B
D�B
C�B
B�B
D�B
D�B
E�B
F�B
F�B
G�B
F�B
F�B
F�B
F�B
G�B
G�B
F�B
F�B
G�B
G�B
G�B
I�B
H�B
I�B
J�B
I�B
I�B
K�B
K�B
J�B
J�B
I�B
I�B
J�B
J�B
J�B
K�B
L�B
K�B
L�B
M�B
M�B
K�B
L�B
M�B
N�B
O�B
N�B
L�B
M�B
K�B
J�B
N�B
P�B
R�B
T�B
S�B
Q�B
S�B
S�B
R�B
R�B
T�B
S�B
T�B
T�B
T�B
T�B
VB
W
B
XB
W
B
XB
YB
YB
YB
XB
YB
YB
ZB
ZB
ZB
YB
YB
YB
ZB
ZB
[#B
[#B
YB
ZB
\)B
[#B
[#B
\)B
[#B
\)B
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
_;B
_;B
`BB
aHB
aHB
aHB
`BB
aHB
aHB
`BB
`BB
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
bNB
bNB
aHB
cTB
dZB
ffB
ffB
ffB
ffB
ffB
ffB
ffB
e`B
ffB
e`B
e`B
e`B
e`B
e`B
dZB
e`B
ffB
e`B
hsB
hsB
gmB
hsB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
gmB
iyB
iyB
iyB
iyB
iyB
l�B
l�B
l�B
l�B
k�B
k�B
k�B
k�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
l�B
l�B
l�B
m�B
n�B
n�B
o�B
o�B
o�B
n�B
o�B
o�B
o�B
n�B
n�B
n�B
n�B
q�B
q�B
q�B
q�B
q�B
q�B
p�B
r�B
r�B
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
t�B
t�B
s�B
u�B
u�B
u�B
v�B
u�B
v�B
v�B
v�B
w�B
x�B
x�B
x�B
w�B
w�B
x�B
x�B
x�B
x�B
x�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
z�B
z�B
y�B
x�B
y�B
z�B
z�B
{�B
{�B
z�B
{�B
{�B
{�B
|�B
|�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
~�B
~�B
� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  BjB~B~BjBjBjBVBjBvBvB�BB}BB�B;dBrB��B��B��B��BרB%FBGEB1�BK�Bg8BYBS�BpoB�AB�KB��B�:B�B�B�?B��B�\B�*B�0B�>B��B��B��B�8B�8B�nB��B�9B�xB�hB�B��B|�Bq�BN�BM�BW�BH�B/iB0�B)yB�B��BB�hB��B��B�B�B��B�YB��B��B}�BkBk�Bc�B[qBRTBP�BPHBE�B-)B*eB#TB9B_BuB �B
��B
�5B
��B
�B
یB
�vB
�9B
��B
�XB
�yB
�NB
~�B
�zB
��B
�MB
s�B
q�B
l=B
nB
qB
h$B
YKB
@OB
:xB
8�B
8lB
.�B
!�B
*eB
�B	��B	�%B	�dB	�	B	�jB	�B	�B	��B	�vB	�.B	�B	�B	� B	�|B	��B	�aB	��B	��B	|�B	z�B	oB	^OB	U�B	M�B	=�B	(�B	;JB	.�B	!�B	jB	
�B�|B�LB��B�TB��B�QBȴB��B�6B��B�B��B�RB�_BxB_pBvB~]Bn�B|6Bv�BraBrGBu�Bp�B`�BYeBZ7BU2BV�BQB]Bf�Bc�B\�BO�B>BBHfBV�BU�BT�BR�BO�BFB@�B?�BB[B5B*B5�B:�BG_BG_BC�BA�B:�BOBBK�BK�BVmBMBA�BQ B[�B^�BZ�B\�B]�B]�B^5Be�Bh$Bg8Bj0BnBlqBrGBxBvzBt�Bw�Bz�Bz�B~wB��B�aB�B��By>B~�BwLBz�BvBw�By�BrGB}�B}�B��BB��B��B��B��B��B��B�;Bp!B}B��B��B�	B�B�B�B�	B�B�B��B�B�B�'B�5B�qB�B�~B�XB��B��B��B��B��B��B��B��B��B�0B�B��B��B�9B�B�)B�BBɆB��BרB߾B�B��B��B�B�GB�TB�lB	AB	MB	GB��B	�B	�B	�B	B	4B	B	�B	!-B	!�B	%B	'B	)*B	)_B	(�B	1[B	6�B	4�B	8lB	7�B	9�B	8�B	6�B	8�B	A�B	A�B	G�B	M�B	S�B	UB	T�B	VB	W$B	VmB	VSB	X_B	`BB	fLB	e�B	g�B	l�B	n�B	o�B	m�B	q�B	z�B	|B	�B	�OB	~BB	�;B	�3B	�%B	�YB	�YB	�YB	�fB	��B	�VB	��B	��B	�~B	��B	��B	��B	�}B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�	B	�-B	�B	�
B	�$B	�8B	�TB	�8B	�*B	�0B	�_B	�WB	�[B	�nB	�ZB	��B	�zB	��B	��B	��B	��B	��B	��B	��B	ðB	��B	��B	��B	��B	��B	��B	��B	�B	�	B	��B	�)B	�B	�BB	�:B	�B	�FB	�&B	�4B	�HB	�4B	�B	�,B	�TB	�FB	�2B	�$B	�+B	�YB	�EB	�B	�OB	�qB	�dB	�xB	�xB	ބB	�\B	�~B	�B	�jB	�\B	�|B	�bB	�B	�B	�B	�B	�B	�B	��B	�B	�B	�B	�B	�B	��B	�B	��B	��B	��B	��B	��B	�B	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	�	B	�	B	��B	�B	�B	��B	�*B	�B	�B	�<B	�B
 B	�B	�(B	�<B	�B	�HB	�B	�BB
 4B	�.B
 4B
;B
'B
 B	�HB
 OB
AB
3B
MB
-B
GB
[B
[B
MB
B
	7B
zB
YB
1B
<B
BB
bB
hB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B

B
�B
�B
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
!�B
!�B
"�B
!�B
!�B
"B
$B
$�B
$&B
$�B
$�B
%B
%�B
'�B
'B
'B
'B
%,B
&2B
&B
%B
&B
'B
'B
&B
&B
&B
'B
(
B
'8B
&LB
'B
'mB
)DB
*B
+B
,"B
+QB
-)B
-)B
./B
/OB
/OB
0;B
0;B
1AB
1AB
1AB
1AB
/iB
.}B
.}B
.}B
2aB
2GB
2aB
2|B
0oB
3hB
5ZB
5tB
6`B
7fB
6zB
6`B
6zB
7�B
8lB
7�B
7fB
8lB
8�B
8lB
9�B
8�B
7�B
7�B
:xB
;B
;B
<jB
<�B
<jB
;B
;�B
:xB
:�B
9�B
9rB
;B
:�B
9�B
:�B
:xB
:�B
:�B
9�B
;B
=�B
<�B
<�B
>�B
>�B
>�B
?�B
?�B
>�B
=�B
<�B
;�B
=�B
A�B
@�B
A�B
C�B
C�B
C�B
B�B
A�B
B�B
B�B
C�B
B�B
C�B
D�B
D�B
D�B
D�B
D�B
D�B
C�B
B�B
D�B
D�B
E�B
F�B
F�B
G�B
F�B
F�B
F�B
F�B
G�B
G�B
F�B
F�B
G�B
G�B
G�B
I�B
H�B
I�B
J�B
J	B
I�B
K�B
K�B
J�B
J�B
I�B
I�B
J�B
J�B
KB
K�B
L�B
LB
MB
NB
NB
L0B
MB
N"B
N�B
PB
N�B
MB
N"B
LB
KDB
OB
QB
SB
T�B
TB
R:B
TB
T,B
S&B
S&B
U2B
TFB
U2B
UB
UB
U2B
VB
W$B
X+B
W?B
X+B
Y1B
YB
YB
X+B
Y1B
Y1B
Z7B
Z7B
Z7B
YKB
Y1B
YKB
Z7B
ZQB
[=B
[=B
YB
ZkB
\CB
[WB
[WB
\CB
[WB
\CB
]IB
]dB
]IB
^OB
^OB
_VB
_pB
_VB
_VB
_VB
_VB
_VB
`\B
abB
aHB
aHB
`\B
abB
abB
`vB
`\B
a|B
bhB
b�B
bhB
bhB
cnB
cTB
c�B
cTB
c�B
c�B
cnB
c�B
cnB
b�B
bhB
a|B
c�B
dtB
ffB
f�B
ffB
ffB
f�B
f�B
ffB
ezB
ffB
ezB
e�B
ezB
e�B
e�B
d�B
e�B
f�B
e�B
h�B
h�B
g�B
h�B
i�B
i�B
i�B
iyB
i�B
i�B
i�B
i�B
i�B
i�B
g�B
i�B
i�B
i�B
i�B
i�B
l�B
l�B
l�B
l�B
k�B
k�B
k�B
k�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
l�B
l�B
l�B
m�B
n�B
n�B
o�B
o�B
o�B
n�B
o�B
o�B
o�B
n�B
n�B
n�B
n�B
q�B
q�B
q�B
q�B
q�B
q�B
p�B
r�B
r�B
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
t�B
t�B
s�B
u�B
u�B
u�B
v�B
u�B
v�B
v�B
v�B
w�B
x�B
x�B
x�B
xB
xB
x�B
x�B
x�B
x�B
x�B
y�B
y�B
y�B
y�B
zB
y�B
y�B
z�B
z�B
zB
y$B
zB
z�B
z�B
|B
|B
z�B
|B
|B
|B
}B
}B
}�B
~B
}�B
}�B
~B
~(B
~B
}�B
~�B
B
� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.05(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201807290040402018072900404020180729004040201807290200292018072902002920180729020029201807300031082018073000310820180730003108  JA  ARFMdecpA19c                                                                20180725093508  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180725003527  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180725003529  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180725003530  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180725003530  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180725003530  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180725003530  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180725003530  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180725003531  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180725003531                      G�O�G�O�G�O�                JA  ARUP                                                                        20180725005557                      G�O�G�O�G�O�                JA  ARUP                                                                        20180727051505                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180725153515  CV  JULD            G�O�G�O�Fã�                JM  ARCAJMQC2.0                                                                 20180728154040  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180728154040  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180728170029  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180729153108  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116231516                      G�O�G�O�G�O�                