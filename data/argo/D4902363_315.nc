CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-12-28T00:36:21Z creation;2018-12-28T00:36:27Z conversion to V3.1;2019-12-19T07:24:42Z update;     
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
resolution        =���   axis      Z        |  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     |  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \t   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     |  `T   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     |  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �,   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     |  �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     |  �h   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     |  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     |  �@   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     |  ̼   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �8   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �H   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �L   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �P   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �T   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �X   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181228003621  20200115131517  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL              ;A   JA  I2_0576_315                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @؛u�ƻ�1   @؛v�Y  @9m��U�=�d?��8�Y1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @9��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dp��Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�C3D� D�� D�  D�@ D� D��3D�3D�C3D� D�� D�  D�@ D� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D��3D��31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @6ff@|��@�ff@�ffA33A?33A_33A33A���A���A���A���Aϙ�Aߙ�AA���B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fC�3C�3C�3C�3C	�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C!�3C#�3C%�3C'�3C)�3C+�3C-�3C/�3C1�3C3�3C5�3C7�3C9�3C;�3C=�3C?�3CA�3CC�3CE�3CG�3CI�3CK�3CM�3CO�3CQ�3CS�3CU�3CW�3CY�3C[�3C]�3C_�3Ca�3Cc�3Ce�3Cg�3Ci�3Ck�3Cm�3Co�3Cq�3Cs�3Cu�3Cw�3Cy�3C{�3C}�3C�3C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D |�D ��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D	|�D	��D
|�D
��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D |�D ��D!|�D!��D"|�D"��D#|�D#��D$|�D$��D%|�D%��D&|�D&��D'|�D'��D(|�D(��D)|�D)��D*|�D*��D+|�D+��D,|�D,��D-|�D-��D.|�D.��D/|�D/��D0|�D0��D1|�D1��D2|�D2��D3|�D3��D4|�D4��D5|�D5��D6|�D6��D7|�D7��D8|�D8��D9|�D9��D:|�D:��D;|�D;��D<|�D<��D=|�D=��D>|�D>��D?|�D?��D@|�D@��DA|�DA��DB|�DB��DC|�DC��DD|�DD��DE|�DE��DF|�DF��DG|�DG��DH|�DH��DI|�DI��DJ|�DJ��DK|�DK��DL|�DL��DM|�DM��DN|�DN��DO|�DO��DP|�DP��DQ|�DQ��DR|�DR��DS|�DS��DT|�DT��DU|�DU��DV|�DV��DW|�DW��DX|�DX��DY|�DY��DZ|�DZ��D[|�D[��D\|�D\��D]|�D]��D^|�D^��D_|�D_��D`|�D`��Da|�Da��Db|�Db��Dc|�Dc��Dd|�Dd��De|�De��Df|�Df��Dg|�Dg��Dh|�Dh��Di|�Di��Dj|�Dj��Dk|�Dk��Dl|�Dl��Dm|�Dm��Dn|�Dn��Do|�Do��Dp|�Dp�fDq|�Dq��Dr|�Dr��Ds|�Ds��Dt|�Dt��Du|�Du��Dv|�Dv��Dw|�Dw��Dx|�Dx��Dy|�Dy��Dz|�Dz��D{|�D{��D||�D|��D}|�D}��D~|�D~��D|�D��D�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��D�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��3D�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD¾fD��fD�>fD�~fDþfD��fD�>fD�~fDľfD��fD�>fD�~fDžfD��fD�>fD�~fDƾfD��fD�>fD�~fDǾfD��fD�>fD�~fDȾfD��fD�>fD�~fDɾfD��fD�>fD�~fDʾfD��fD�>fD�~fD˾fD��fD�>fD�~fD̾fD��fD�>fD�~fD;fD��fD�>fD�~fDξfD��fD�>fD�~fDϾfD��fD�>fD�~fDоfD��fD�>fD�~fDѾfD��fD�>fD�~fDҾfD��fD�>fD�~fDӾfD��fD�>fD�~fDԾfD��fD�>fD�~fDվfD��fD�>fD�~fD־fD��fD�>fD�~fD׾fD��fD�>fD�~fDؾfD��fD�>fD�~fDپfD��fD�>fD�~fDھfD��fD�>fD�~fD۾fD��fD�>fD�~fDܾfD��fD�>fD�~fDݾfD��fD�>fD�~fD޾fD��fD�>fD�~fD߾fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�A�D�~fD�fD��fD�>fD�~fD���D��D�A�D�~fD��fD��fD�>fD�~fD�fD��fD�>fD�{3D�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD��fD��fD�>fD�{3D��fD��fD�>fD�~fD��fD��fD�>fD���D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�Q�A�O�A�M�A�I�A�5?A�/A�/A�+A�&�A�&�A�(�A�(�A�$�A�"�A��A��A�JA���A��A��`A��HA��/A���A��jA���A�jA�^5A�VA�G�A�;dA�+A�
=A���A���A��A��TA��
A�A���A�r�A��A���A�bNA�bNA�p�A���A�p�A�(�A�A��A��/A���A���A���A���A�K�A�hsA���A��RA���A��A��A���A�/A��A�r�A��`A�dZA�&�A�-A�\)A��+A���A�`BA�"�A�A�A��uA���A��!A�K�A���A���A�M�A���A�bA���A�A�VA��-A�Q�A��A���A�-A���A�ffA�1A���A��uA���A�M�A��yA�{A��DA�A�oA���A�\)A~��A}�A|��Ay��Aw/Au��Aut�Atn�As+Ar��Ar��Ar��Arv�ArffArA�Ar �Aq�TAq��AqhsAp�yAoXAk��Ak/Ah��AeO�Ad��Ac�
A`~�A_�;A_&�A^A]|�A\Q�AZ1'AX�jAW�PAU7LAS�
ARE�AP�AP�!AP��AO�AMdZAL�!AL-AK�#AKt�AHM�AFz�AE�AD�/ADĜADffAC��AA�;A@ZA??}A=��A<�yA<�DA<bNA;�
A:�+A8ZA6ĜA6jA6=qA5�PA4�A3ƨA2�A1|�A0��A.��A-dZA,�+A+�A+dZA* �A(�`A'�A&�A%��A%oA$�/A#��A"��A!��A �!A I�Ap�A=qAVAJA�^A��A$�A  A�A��Al�A�A�A�
AA�RA��Az�AVA�;A`BA�/AQ�AS�A;dA^5A�A�wA��A�AXA��AbA�-A"�Av�A
�A
z�A
�A	��A�/A=qA  At�AVA�A��A^5A��AG�A��A �A&�A�RA$�AA�A��A��A��A�7A?}A �jA 5?A bA @��m@��w@���@�dZ@��@���@�%@��j@�M�@���@�Ĝ@�t�@�!@��T@�%@��@��y@�7L@��
@�K�@�v�@�`B@�J@�dZ@�@�z�@�n�@۶F@�{@�O�@؛�@�b@ץ�@֟�@��@���@���@���@д9@�+@�ȴ@ͩ�@�j@�"�@�~�@�$�@�@��@��T@�`B@ȣ�@�b@�o@��@�b@��@���@�S�@��@�z�@�Z@� �@���@�
=@���@�J@��9@��D@�bN@�Q�@�(�@���@��P@��@��P@�O�@��@�33@��@��!@��+@�5?@��@���@���@���@�z�@���@��@�t�@��@���@�ff@���@�Z@���@�ff@��7@�/@�V@���@���@��j@��@���@�r�@�j@�Q�@�I�@�I�@�9X@�9X@�1'@� �@��
@�t�@�33@��!@�{@��T@���@��h@���@�p�@���@���@���@���@�
=@���@�V@��`@��u@� �@�o@���@��D@�bN@�Z@�Q�@�A�@�9X@�1'@�1'@�(�@���@�;d@���@�v�@�{@��@��#@��-@���@�x�@�X@�X@�?}@�/@��@��9@�9X@��@��
@�|�@�l�@�C�@�@��R@�^5@�-@�`B@�bN@�+@�v�@��^@�&�@��@�Ĝ@��9@��u@�j@�1'@�1@�1@���@��@��m@��;@��
@��w@�|�@��@�^5@�@���@��@���@��j@��u@��D@�r�@�I�@�9X@�9X@� �@�b@��m@���@���@��w@���@�dZ@�@��!@�ff@�E�@��@���@�?}@� �@~E�@|��@{ƨ@{o@z��@z-@y�^@x��@w�@w�P@w|�@wK�@wK�@wK�@w+@w�@w
=@w
=@w�@w�@w�@w�@w�@w�@w
=@w�@w+@w;d@w;d@w
=@v�R@v{@u?}@t�D@t�@s��@r��@q��@p��@p�9@p�@pr�@pA�@p �@o�;@o�@ol�@o
=@nȴ@n��@nE�@n@m@mp�@mO�@m�@l�@l�@k�m@k��@k�m@k��@k@j=q@ihs@i�@h��@h��@hĜ@h�9@h�u@hbN@hQ�@hb@g��@f@e�@bn�@`�u@_�;@_�P@_\)@_;d@_
=@^�y@^ȴ@^��@^E�@]�-@\(�@[��@[S�@Z~�@X��@Xr�@X1'@X  @W�@W\)@V��@Vȴ@V��@V��@V�R@V��@VE�@V@U�h@U�@T�j@T��@T�D@T9X@SdZ@S33@S@R��@R�!@R��@R^5@Q�#@Q7L@Pr�@Pb@O�@O��@O\)@Nȴ@N�R@N�+@N$�@M��@M�-@M`B@L��@L��@Lz�@LI�@L1@K�F@K��@K�F@Kƨ@Kƨ@K��@Kt�@KdZ@KS�@K33@K@J��@J��@J�\@I��@Ix�@I�@Hr�@Hb@Gl�@F@E�@D��@D�D@D(�@D�@C��@C��@C�m@C��@C�@CdZ@B�!@B=q@A�#@A��@@�`@@Ĝ@@�9@@bN@@A�@@ �@@b@@b@?�@?�w@?|�@?K�@>��@>��@>E�@=@=�h@=`B@<��@<�/@<�/@<�/@<��@<��@<�j@<�D@<(�@;��@;��@;��@;�@;t�@;dZ@;dZ@;S�@;C�@;"�@:�H@:��@:�H@:��@:�H@:�H@:��@:=q@9�#@9�^@9�^@9��@9�7@9X@9G�@97L@9�@8�`@8�u@8r�@8bN@8Q�@8Q�@81'@8 �@7�@7|�@7\)@7;d@7
=@6�y@6�@6ȴ@6ff@5��@5�@5`B@4�@4�D@4Z@3��@3o@2�\@2M�@2=q@2-@1%@0�u@0�@0�@0r�@0bN@0bN@0Q�@0Q�@01'@/�w@/;d@/
=@/
=@.��@.�@.��@.v�@.ff@.V@.V@.V@.E�@.5?@.5?@.$�@.$�@.{@.@-�T@-O�@,�@,z�@,j@,1@+�
@+��@+o@*�\@*-@)��@)�#@)7L@(�9@(r�@'�@'|�@'K�@&��@&{@%��@%�h@%O�@$��@$��@$Z@$�@$1@#ƨ@#dZ@#"�@"�H@"��@"~�@"=q@!�#@!��@!G�@ �`@ ��@ �u@ 1'@  �@ b@ b@ b@ b@ b@ b@   @   @ b@�;@�@�R@�+@E�@�T@?}@�D@j@1@��@@��@~�@-@�@�@J@J@J@��@�#@�^@x�@G�@&�@%@��@�@Q�@�@K�@;d@
=@�y@ȴ@�R@��@��@$�@@�@��@O�@�j@z�@Z@9X@�F@S�@@�@��@��@^5@�@�@�^@��@x�@X@X@G�@&�@&�@�@%@��@�u@Q�@b@�@l�@+@
=@��@�y@�@�@�@ȴ@�R@E�@��@�-@�-@��@�h@�@�@p�@p�@p�@`B@O�@/@V@�/@z�@I�@9X@9X@(�@1@��@�
@�F@C�@o@
�@
��@
�!@
��@
�\@
~�@
n�@
M�@
�@	hs@	G�@	7L@	7L@	7L@	�@��@�`@�`@�9@bN@bN@Q�@1'@1'@1'@1'@1'@1'@ �@b@b@�@�w@�w@�@��@|�@K�@;d@;d@+@+@�@
=@�@�R@��@v�@E�@@�@�T@��@@��@`B@�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�Q�A�O�A�M�A�I�A�5?A�/A�/A�+A�&�A�&�A�(�A�(�A�$�A�"�A��A��A�JA���A��A��`A��HA��/A���A��jA���A�jA�^5A�VA�G�A�;dA�+A�
=A���A���A��A��TA��
A�A���A�r�A��A���A�bNA�bNA�p�A���A�p�A�(�A�A��A��/A���A���A���A���A�K�A�hsA���A��RA���A��A��A���A�/A��A�r�A��`A�dZA�&�A�-A�\)A��+A���A�`BA�"�A�A�A��uA���A��!A�K�A���A���A�M�A���A�bA���A�A�VA��-A�Q�A��A���A�-A���A�ffA�1A���A��uA���A�M�A��yA�{A��DA�A�oA���A�\)A~��A}�A|��Ay��Aw/Au��Aut�Atn�As+Ar��Ar��Ar��Arv�ArffArA�Ar �Aq�TAq��AqhsAp�yAoXAk��Ak/Ah��AeO�Ad��Ac�
A`~�A_�;A_&�A^A]|�A\Q�AZ1'AX�jAW�PAU7LAS�
ARE�AP�AP�!AP��AO�AMdZAL�!AL-AK�#AKt�AHM�AFz�AE�AD�/ADĜADffAC��AA�;A@ZA??}A=��A<�yA<�DA<bNA;�
A:�+A8ZA6ĜA6jA6=qA5�PA4�A3ƨA2�A1|�A0��A.��A-dZA,�+A+�A+dZA* �A(�`A'�A&�A%��A%oA$�/A#��A"��A!��A �!A I�Ap�A=qAVAJA�^A��A$�A  A�A��Al�A�A�A�
AA�RA��Az�AVA�;A`BA�/AQ�AS�A;dA^5A�A�wA��A�AXA��AbA�-A"�Av�A
�A
z�A
�A	��A�/A=qA  At�AVA�A��A^5A��AG�A��A �A&�A�RA$�AA�A��A��A��A�7A?}A �jA 5?A bA @��m@��w@���@�dZ@��@���@�%@��j@�M�@���@�Ĝ@�t�@�!@��T@�%@��@��y@�7L@��
@�K�@�v�@�`B@�J@�dZ@�@�z�@�n�@۶F@�{@�O�@؛�@�b@ץ�@֟�@��@���@���@���@д9@�+@�ȴ@ͩ�@�j@�"�@�~�@�$�@�@��@��T@�`B@ȣ�@�b@�o@��@�b@��@���@�S�@��@�z�@�Z@� �@���@�
=@���@�J@��9@��D@�bN@�Q�@�(�@���@��P@��@��P@�O�@��@�33@��@��!@��+@�5?@��@���@���@���@�z�@���@��@�t�@��@���@�ff@���@�Z@���@�ff@��7@�/@�V@���@���@��j@��@���@�r�@�j@�Q�@�I�@�I�@�9X@�9X@�1'@� �@��
@�t�@�33@��!@�{@��T@���@��h@���@�p�@���@���@���@���@�
=@���@�V@��`@��u@� �@�o@���@��D@�bN@�Z@�Q�@�A�@�9X@�1'@�1'@�(�@���@�;d@���@�v�@�{@��@��#@��-@���@�x�@�X@�X@�?}@�/@��@��9@�9X@��@��
@�|�@�l�@�C�@�@��R@�^5@�-@�`B@�bN@�+@�v�@��^@�&�@��@�Ĝ@��9@��u@�j@�1'@�1@�1@���@��@��m@��;@��
@��w@�|�@��@�^5@�@���@��@���@��j@��u@��D@�r�@�I�@�9X@�9X@� �@�b@��m@���@���@��w@���@�dZ@�@��!@�ff@�E�@��@���@�?}@� �@~E�@|��@{ƨ@{o@z��@z-@y�^@x��@w�@w�P@w|�@wK�@wK�@wK�@w+@w�@w
=@w
=@w�@w�@w�@w�@w�@w�@w
=@w�@w+@w;d@w;d@w
=@v�R@v{@u?}@t�D@t�@s��@r��@q��@p��@p�9@p�@pr�@pA�@p �@o�;@o�@ol�@o
=@nȴ@n��@nE�@n@m@mp�@mO�@m�@l�@l�@k�m@k��@k�m@k��@k@j=q@ihs@i�@h��@h��@hĜ@h�9@h�u@hbN@hQ�@hb@g��@f@e�@bn�@`�u@_�;@_�P@_\)@_;d@_
=@^�y@^ȴ@^��@^E�@]�-@\(�@[��@[S�@Z~�@X��@Xr�@X1'@X  @W�@W\)@V��@Vȴ@V��@V��@V�R@V��@VE�@V@U�h@U�@T�j@T��@T�D@T9X@SdZ@S33@S@R��@R�!@R��@R^5@Q�#@Q7L@Pr�@Pb@O�@O��@O\)@Nȴ@N�R@N�+@N$�@M��@M�-@M`B@L��@L��@Lz�@LI�@L1@K�F@K��@K�F@Kƨ@Kƨ@K��@Kt�@KdZ@KS�@K33@K@J��@J��@J�\@I��@Ix�@I�@Hr�@Hb@Gl�@F@E�@D��@D�D@D(�@D�@C��@C��@C�m@C��@C�@CdZ@B�!@B=q@A�#@A��@@�`@@Ĝ@@�9@@bN@@A�@@ �@@b@@b@?�@?�w@?|�@?K�@>��@>��@>E�@=@=�h@=`B@<��@<�/@<�/@<�/@<��@<��@<�j@<�D@<(�@;��@;��@;��@;�@;t�@;dZ@;dZ@;S�@;C�@;"�@:�H@:��@:�H@:��@:�H@:�H@:��@:=q@9�#@9�^@9�^@9��@9�7@9X@9G�@97L@9�@8�`@8�u@8r�@8bN@8Q�@8Q�@81'@8 �@7�@7|�@7\)@7;d@7
=@6�y@6�@6ȴ@6ff@5��@5�@5`B@4�@4�D@4Z@3��@3o@2�\@2M�@2=q@2-@1%@0�u@0�@0�@0r�@0bN@0bN@0Q�@0Q�@01'@/�w@/;d@/
=@/
=@.��@.�@.��@.v�@.ff@.V@.V@.V@.E�@.5?@.5?@.$�@.$�@.{@.@-�T@-O�@,�@,z�@,j@,1@+�
@+��@+o@*�\@*-@)��@)�#@)7L@(�9@(r�@'�@'|�@'K�@&��@&{@%��@%�h@%O�@$��@$��@$Z@$�@$1@#ƨ@#dZ@#"�@"�H@"��@"~�@"=q@!�#@!��@!G�@ �`@ ��@ �u@ 1'@  �@ b@ b@ b@ b@ b@ b@   @   @ b@�;@�@�R@�+@E�@�T@?}@�D@j@1@��@@��@~�@-@�@�@J@J@J@��@�#@�^@x�@G�@&�@%@��@�@Q�@�@K�@;d@
=@�y@ȴ@�R@��@��@$�@@�@��@O�@�j@z�@Z@9X@�F@S�@@�@��@��@^5@�@�@�^@��@x�@X@X@G�@&�@&�@�@%@��@�u@Q�@b@�@l�@+@
=@��@�y@�@�@�@ȴ@�R@E�@��@�-@�-@��@�h@�@�@p�@p�@p�@`B@O�@/@V@�/@z�@I�@9X@9X@(�@1@��@�
@�F@C�@o@
�@
��@
�!@
��@
�\@
~�@
n�@
M�@
�@	hs@	G�@	7L@	7L@	7L@	�@��@�`@�`@�9@bN@bN@Q�@1'@1'@1'@1'@1'@1'@ �@b@b@�@�w@�w@�@��@|�@K�@;d@;d@+@+@�@
=@�@�R@��@v�@E�@@�@�T@��@@��@`B@�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 Bp�Bo�Bp�Bn�Bo�Bo�Bo�Bp�Bp�Bp�Bp�Bo�Bp�Br�Bs�Bv�B{�B�B�B�B�B�B�B~�B|�B�B�B� B� B~�B}�B� B�B�B~�B|�Bz�Bw�Bu�Bo�Bk�B_;BG�B�BG�Bx�Bq�BW
BS�BL�B:^BO�B^5BcTBcTBYBVBT�BP�BiyBaHBG�BO�BB�B6FB?}B>wBA�B)�B"�B%�B�B%�B�B+B�B�B��B�B��BɺB��BiyB~�BffBL�BO�BE�B"�B\BB
��B
�;B
��B
�#B
�ZB
��B
�3B
��B
��B
�PB
k�B
S�B
[#B
iyB
_;B
C�B
>wB
-B
VB
B
PB
�B
�B
DB
�B
�B
�B
�B
�B
�B
oB
PB
+B	��B	�B	��B	��B	�dB	��B	s�B	��B	��B	ffB	�PB	�B	x�B	u�B	cTB	G�B	E�B	C�B	-B	,B	2-B	(�B	:^B	7LB	%�B	B	�B	�B	uB	B�#B�5B��B��B��B�B�;BƨBƨBǮB�}B��B��B��B�qB�B��B��B�3B�-B��B��B�{B�7B�VB�PBy�Bw�B�B�%B�Br�Bx�Bx�Br�Bs�B~�B�Br�Bp�Bk�BjBp�BgmBVBL�BiyBhsB^5B]/Bk�Bk�BgmB_;B\)B[#BQ�BQ�B\)BaHB]/BZBQ�BL�BI�BA�B33B �B:^BF�BK�BL�BJ�BF�B>wB6FB=qB6FB1'B!�B9XB8RB49B/B1'B7LB1'B5?B6FB6FB2-B-B%�B+B �B�B)�B(�B33B5?B49B2-B49B1'B,B(�B'�B33B49B33B2-B1'B.B)�B �BbB��BuB'�B(�B"�B)�B)�B'�B%�B%�B�B%�B+B%�B�B%BVB�B�B�B�B!�B33B5?B7LB6FB2-B49B5?B)�B�B"�B+B<jB6FB49B:^BD�BK�BM�BM�BK�BF�BB�BC�B?}B>wB9XB6FBF�BC�BB�B[#BaHB`BB]/B]/B^5B_;B[#Bk�Bm�Bl�BjBdZBe`B[#BVB_;Bo�B|�B�B�+B�+B�%B�B�1B�\B�JB�1B�B}�B� B��B��B��B��B��B��B��B�B�LB�dB�jB�jB�wB�}B�wB�wB��B��B��B��B��B��B��B��B�wB�qB��B�}B��BȴBǮB��B��BǮBĜB�wB��B��B��B��B��B�/B�B�
B��B��B�/B�B�B�B�B�B��B�B�B�B�B�B��B��B��B��B��B��B��B��B	  B��B��B��B��B��B��B��B��B	B	B��B	  B	  B	B��B��B	B	oB	�B	 �B	(�B	-B	.B	.B	.B	0!B	2-B	5?B	49B	5?B	5?B	5?B	49B	33B	1'B	0!B	49B	<jB	>wB	B�B	I�B	N�B	P�B	R�B	R�B	T�B	YB	ZB	YB	[#B	[#B	\)B	^5B	]/B	\)B	[#B	\)B	bNB	ffB	iyB	iyB	hsB	iyB	ffB	p�B	w�B	�B	�+B	�DB	�JB	�JB	�JB	�\B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�'B	�-B	�9B	�?B	�FB	�FB	�LB	�LB	�LB	�RB	�RB	�XB	�^B	�^B	�^B	�XB	��B	ÖB	B	��B	��B	��B	��B	ŢB	ƨB	ǮB	ȴB	ɺB	ɺB	ɺB	��B	ȴB	ŢB	B	ǮB	ŢB	��B	�B	�/B	�;B	�;B	�;B	�BB	�BB	�;B	�5B	�)B	�B	�ZB	�NB	�BB	�;B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
%B
%B
1B
	7B
	7B
JB
PB
VB
PB
PB
VB
\B
\B
\B
VB
VB
\B
VB
DB
\B
\B
VB
\B
VB
PB
hB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
!�B
 �B
!�B
"�B
"�B
"�B
"�B
"�B
!�B
"�B
"�B
"�B
#�B
#�B
%�B
%�B
%�B
'�B
(�B
(�B
'�B
'�B
'�B
&�B
%�B
'�B
'�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
(�B
+B
+B
+B
+B
)�B
(�B
&�B
(�B
,B
,B
,B
,B
,B
,B
,B
,B
,B
+B
-B
-B
.B
.B
-B
-B
,B
,B
-B
.B
.B
.B
.B
-B
,B
)�B
/B
.B
-B
.B
/B
,B
.B
/B
1'B
2-B
0!B
,B
1'B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
49B
33B
33B
6FB
8RB
8RB
7LB
7LB
8RB
9XB
:^B
:^B
:^B
:^B
:^B
:^B
:^B
:^B
:^B
9XB
8RB
6FB
7LB
8RB
;dB
9XB
;dB
:^B
9XB
9XB
;dB
<jB
<jB
:^B
<jB
>wB
?}B
@�B
B�B
@�B
A�B
D�B
D�B
E�B
E�B
E�B
F�B
G�B
H�B
G�B
G�B
H�B
I�B
J�B
I�B
J�B
J�B
J�B
K�B
K�B
M�B
M�B
M�B
O�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
O�B
M�B
J�B
N�B
O�B
O�B
N�B
N�B
N�B
R�B
Q�B
Q�B
R�B
VB
VB
VB
YB
YB
YB
YB
YB
XB
XB
XB
W
B
XB
YB
YB
XB
XB
XB
W
B
YB
\)B
[#B
[#B
\)B
\)B
\)B
\)B
ZB
\)B
\)B
[#B
[#B
[#B
]/B
^5B
]/B
\)B
^5B
_;B
`BB
`BB
`BB
`BB
`BB
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
cTB
bNB
bNB
bNB
bNB
cTB
bNB
cTB
ffB
ffB
ffB
ffB
ffB
ffB
ffB
e`B
cTB
cTB
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
gmB
gmB
gmB
ffB
ffB
hsB
iyB
iyB
iyB
hsB
hsB
hsB
gmB
ffB
hsB
jB
jB
jB
jB
k�B
k�B
jB
jB
hsB
gmB
l�B
m�B
n�B
m�B
l�B
m�B
m�B
m�B
l�B
l�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
n�B
n�B
n�B
n�B
o�B
n�B
o�B
o�B
p�B
p�B
p�B
p�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
q�B
q�B
q�B
q�B
q�B
q�B
s�B
s�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 Bp�Bo�Bp�Bn�Bo�Bo�Bo�Bp�Bp�Bp�Bp�Bo�Bp�Br�Bs�Bv�B|B�-B�B�B�3B�-B�;BHB}VB� B� B�B�B.B~(B�B�B� BB}B{Bx8BvFBpoBlqB`�BJ�B/BJ�Bx�Br�BY�BV�BO�B>�BRoB`\Bd�Bd�B[#BW�BV�BR�Bi�Ba�BI�BQ BDMB8�B@�B?�BB[B,WB$�B'�BsB&fB �B	�B��B��B��B��B�pB�^B�CBo5B� Bi�BPHBQ�BGzB&B�BzB
�$B
�B
�B
یB
�B
�@B
��B
��B
��B
��B
o5B
WYB
]/B
jB
`vB
E�B
@ B
/B
�B
�B
�B
!B
�B
�B
�B
�B
	B
�B
�B
�B
�B
�B
�B	��B	�B	�(B	��B	��B	��B	x8B	�pB	��B	i�B	��B	�%B	zDB	v�B	e,B	JrB	G�B	EmB	/�B	-�B	3�B	*�B	:�B	7�B	'B	MB	QB	QB	,B	SB�B�\B��B��B�*B�iB��B�7BȚB�7B�oB˒B�vB�0B��B�B�QB��B��B��B��B��B�B�)B��B��B|PBy�B�9B�B�%Bt�Bz^BzDBt9Bu?B�B��BtTBq�Bm)Bk�BqvBh�BW�BO(Bi�BiB_VB^jBk�Bk�Bg�B`B\�B[�BS@BSB\�Ba|B]�BZ�BR�BM�BJ�BB�B4�B#�B;BG_BLBMBKBG+B?}B7fB>B7LB2aB#�B9�B9	B5B0;B1�B7�B1�B5�B6�B6�B2�B-�B&�B+�B!�B�B*�B)�B3�B5tB4nB2|B4TB1vB,�B)�B(�B3hB4nB3hB2aB1[B.cB*eB!bB�B��B�B(sB)�B#�B*�B*�B(�B&�B&�B B&�B+�B&�B�B�B�B�B�BBKB#B3�B5�B7�B6�B2�B4�B5�B*�BVB$ZB,"B<�B72B5?B;dBEBLBNBM�BLBG+BC-BDMB@iB?}B:�B7�BGzBEBDB[�Ba|B`�B]�B]�B^�B_�B\CBk�Bm�Bl�Bj�Bd�Be�B\�BW�B`�Bp�B}qB�SB�zB�_B��B��B��B�vB��B��B��BcB��B�B�B�,B�VB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B��B��B��B��B��B�B�}B��B�FBϑB��BӏB�dBڠB׍B��B�B�B�B��B��B��B��B��B��B��B��B�IB�9B�B�*B�.B�B�(B�B�.B�.B	 4B�.B�.B�(B�JB�DB�.B�BB�]B	 B	UB�HB	 OB	 OB	UB��B��B	B	�B	B	!-B	)*B	-)B	./B	.IB	.cB	0UB	2GB	5?B	4TB	5tB	5ZB	5ZB	4TB	3hB	1vB	0�B	4�B	<�B	>�B	B�B	I�B	N�B	Q B	S&B	SB	U2B	Y1B	ZB	Y1B	[WB	[=B	\CB	^OB	]dB	\]B	[�B	\xB	b�B	f�B	i�B	i�B	h�B	i�B	gRB	qvB	xlB	�MB	�zB	�xB	��B	�~B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�;B	�$B	�CB	�5B	�;B	�[B	�GB	�TB	�tB	�`B	�zB	��B	��B	�fB	��B	�lB	��B	��B	��B	��B	��B	��B	ðB	��B	��B	��B	��B	��B	żB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�GB	�KB	ƎB	�jB	�KB	�IB	�pB	�pB	�pB	�\B	�\B	�VB	�jB	�xB	ںB	�B	�B	��B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�2B	�8B	�B	�.B	�.B	�(B	�(B
B
3B
GB
3B
9B
MB
MB
?B
YB
fB
	RB
	RB
JB
PB
pB
jB
jB
pB
\B
vB
�B
pB
pB
�B
pB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
!�B
 �B
!�B
"�B
"�B
"�B
#B
#B
!�B
"�B
"�B
# B
$&B
$B
%�B
%�B
%�B
(
B
)B
(�B
'�B
(
B
(
B
'B
&B
($B
($B
*B
)�B
)�B
*B
)�B
*B
*B
*0B
)B
+B
+B
+B
+B
)�B
)B
'8B
)B
,B
,B
,"B
,"B
,=B
,"B
,"B
,"B
,=B
+B
-)B
-)B
.B
./B
-)B
-CB
,"B
,=B
-)B
./B
./B
./B
./B
-)B
,WB
*KB
/B
./B
-]B
.IB
/5B
,WB
.IB
/OB
1AB
2GB
0;B
,qB
1[B
5ZB
5?B
5?B
5?B
5ZB
5?B
5?B
4TB
3hB
3hB
6`B
8lB
8RB
7fB
7fB
8�B
9rB
:^B
:^B
:^B
:xB
:^B
:xB
:^B
:^B
:xB
9rB
8lB
6�B
7�B
8�B
;dB
9�B
;�B
:xB
9�B
9�B
;B
<�B
<�B
:�B
<�B
>�B
?�B
@�B
B�B
@�B
A�B
D�B
D�B
E�B
E�B
E�B
F�B
G�B
H�B
G�B
G�B
H�B
I�B
J�B
I�B
J�B
J�B
J�B
K�B
K�B
M�B
M�B
NB
O�B
Q B
P�B
P�B
P�B
Q B
P�B
P�B
Q B
O�B
M�B
K)B
N�B
O�B
O�B
OB
OB
O(B
R�B
R B
R B
S@B
VB
VB
VB
Y1B
YB
YB
Y1B
YB
XB
X+B
X+B
W?B
X+B
Y1B
Y1B
XEB
XEB
X+B
W?B
Y1B
\)B
[=B
[=B
\)B
\)B
\CB
\]B
ZQB
\)B
\CB
[=B
[WB
[qB
]dB
^OB
]dB
\]B
^OB
_VB
`BB
`\B
`\B
`\B
`\B
a|B
a|B
bhB
b�B
cnB
cTB
cTB
cnB
cnB
cnB
cTB
bhB
bhB
b�B
bhB
c�B
b�B
c�B
f�B
ffB
f�B
ffB
ffB
f�B
f�B
ezB
c�B
cnB
gmB
g�B
gmB
gmB
hsB
hsB
hsB
hsB
hsB
hsB
g�B
g�B
g�B
f�B
f�B
h�B
iyB
i�B
i�B
h�B
h�B
h�B
g�B
f�B
h�B
jB
j�B
j�B
j�B
k�B
k�B
j�B
j�B
h�B
g�B
l�B
m�B
n�B
m�B
l�B
m�B
m�B
m�B
l�B
l�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
n�B
n�B
n�B
n�B
o�B
n�B
o�B
o�B
p�B
p�B
p�B
p�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
q�B
q�B
q�B
q�B
q�B
q�B
s�B
s�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.05(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201901010037322019010100373220190101003732201901010200162019010102001620190101020016201901020024372019010200243720190102002437  JA  ARFMdecpA19c                                                                20181228093620  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20181228003621  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20181228003625  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20181228003625  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20181228003626  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20181228003626  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20181228003626  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20181228003626  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20181228003626  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20181228003627                      G�O�G�O�G�O�                JA  ARUP                                                                        20181228005558                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20181228153421  CV  JULD            G�O�G�O�F�۱                JM  ARCAJMQC2.0                                                                 20181231153732  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20181231153732  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20181231170016  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20190101152437  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115131517                      G�O�G�O�G�O�                