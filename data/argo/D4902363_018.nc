CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2016-07-20T00:35:14Z creation;2016-07-20T00:35:16Z conversion to V3.1;2019-12-19T08:35:12Z update;     
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
_FillValue                    �<Argo profile    3.1 1.2 19500101000000  20160720003514  20200115101518  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA  I2_0576_018                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @׼��tn�1   @׼�����@<:)�y���dte��O1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @333@�  @�  A   A   A@  A`  A�  A�33A�  A�  A�  A�  A���A�33A�33B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�ٚC�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  Dy�D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df�fDg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׃3D�� D�3D�C3D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@0  @|��@�ff@�ffA33A?33A_33A33A���A���A���A���Aϙ�A�ffA���A���B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fC�3C�3C�3C�3C	�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C!�3C#�3C%�3C'�3C)�3C+�3C-�3C/�3C1�3C3�3C5�3C7�3C9�3C;�3C=�3C?�3CA�3CC�3CE�3CG�3CI�3CK�3CM�3CO�3CQ�3CS�3CU�3CW�3CY�3C[�3C]�3C_�3Ca�3Cc�3Ce�3Cg�3Ci�3Ck�3Cm�3Co�3Cq�3Cs�3Cu�3Cw�3Cy�3C{�3C}�3C�3C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�fC��3C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D |�D ��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D	|�D	��D
|�D
��D|�D��D|�D��D|�D��DvfD��D|�D��D|�D�fD|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D |�D ��D!|�D!��D"|�D"��D#|�D#��D$|�D$��D%|�D%��D&|�D&��D'|�D'��D(|�D(��D)|�D)��D*|�D*��D+|�D+��D,|�D,��D-|�D-��D.|�D.��D/|�D/��D0|�D0��D1|�D1��D2|�D2��D3|�D3��D4|�D4��D5|�D5��D6|�D6��D7|�D7��D8|�D8��D9|�D9��D:|�D:��D;|�D;��D<|�D<��D=|�D=��D>|�D>��D?|�D?��D@|�D@��DA|�DA��DB|�DB��DC|�DC��DD|�DD��DE|�DE��DF|�DF��DG|�DG��DH|�DH��DI|�DI��DJ|�DJ��DK|�DK��DL|�DL��DM|�DM��DN|�DN��DO|�DO��DP|�DP��DQ|�DQ��DR|�DR��DS|�DS��DT|�DT��DU|�DU��DV|�DV��DW|�DW��DX|�DX��DY|�DY��DZ|�DZ��D[|�D[��D\|�D\��D]|�D]��D^|�D^��D_|�D_��D`|�D`��Da|�Da��Db|�Db��Dc|�Dc��Dd|�Dd��De|�De��Df�3Df��Dg|�Dg��Dh|�Dh��Di|�Di��Dj|�Dj��Dk|�Dk��Dl|�Dl��Dm|�Dm��Dn|�Dn��Do|�Do��Dp|�Dp��Dq|�Dq��Dr|�Dr��Ds|�Ds��Dt|�Dt��Du|�Du��Dv|�Dv��Dw|�Dw��Dx|�Dx��Dy|�Dy��Dz|�Dz��D{|�D{��D||�D|��D}|�D}��D~|�D~��D|�D��D�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��D�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD¾fD��fD�>fD�~fDþfD��fD�>fD�~fDľfD��fD�>fD�~fDžfD��fD�>fD�~fDƾfD��fD�>fD�~fDǾfD��fD�>fD�~fDȾfD��fD�>fD�~fDɾfD��fD�>fD�~fDʾfD��fD�>fD�~fD˾fD��fD�>fD�~fD̾fD��fD�>fD�~fD;fD��fD�>fD�~fDξfD��fD�>fD�~fDϾfD��fD�>fD�~fDоfD��fD�>fD�~fDѾfD��fD�>fD�~fDҾfD��fD�>fD�~fDӾfD��fD�>fD�~fDԾfD��fD�>fD�~fDվfD��fD�>fD�~fD־fD��fD�>fDׁ�D׾fD��D�A�D�~fDؾfD��fD�>fD�~fDپfD��fD�>fD�~fDھfD��fD�>fD�~fD۾fD��fD�>fD�~fDܾfD��fD�>fD�~fDݾfD��fD�>fD�~fD޾fD��fD�>fD�~fD߾fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD��fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD���D�� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��A��A��A��A��A��A��A���A��A��A���A��yA��HAʾwAɶFA��
A���A���A��A��wA���A�ƨA�  A���A��A���A��
A���A��TA�ffA�ZA��HA�VA�`BA�v�A���A�$�A���A��wA�`BA�^5A��/A��uA��A� �A��jA���A��DA�=qA��A�I�A�z�A�G�A���A�33A�A���A�K�A���A��hA�ffA���A��-A�bNA�~�A��^A��A�=qA�"�A��A�%A�=qA�5?A��-A���A��A��A��\A��7A��wA�|�A��A~�!A|�uA{�mA{`BA{�AyO�Aw�;Av�!Au�mAu�7At�uAs7LAq��ApM�Ao
=Am`BAl$�Ak�wAk`BAjz�Ah{Af��Ae`BAc�7Ab�HAbffAa��AaS�Aa�A`ȴA_|�A^��A]t�A\A�A[33AZVAYx�AXffAV��AU��AUt�ATz�ASXAQ�;AQdZAP�AP�\AO�AO33AN�AM/AL�RAL �AJȴAHȴAHAG��AF�HAFQ�AE�TAE��AE�AD�RADQ�AC��AC
=AB��AA��A@�A@I�A?dZA>��A=�#A<�uA;C�A:n�A9��A9
=A8�\A7�#A7?}A5��A4�!A3��A2ȴA1�TA0~�A/�-A.�9A-�A,��A+��A+&�A*{A(�9A'XA&ZA&A%�A$^5A#"�A"^5A"A!?}A��A(�A|�A��A�!A�\AVA �AA��A`BA��AhsAȴAM�A�;A��A�/AA��AG�A/A�AQ�A
=AE�AAt�A��A��A
=A �A�A\)A
ĜA	�mA	%A�mA�A��AjA  A�hA/A�A�AZA�TA �A��A��A�7@�v�@��@�"�@�=q@��@��h@�V@�r�@�ȴ@���@�@�h@�+@�+@�5?@���@��`@�P@�ȴ@�5?@���@�ff@�Z@�~�@�A�@�dZ@�ff@ݙ�@ܛ�@�j@��@�+@�ȴ@أ�@�o@պ^@��@�  @�dZ@Ѳ-@ύP@�5?@�hs@�r�@��@�|�@�V@���@�p�@���@�$�@�Ĝ@Ý�@�"�@�ȴ@�ff@�J@��7@��@��@�A�@��P@���@��+@�ff@��@�`B@�?}@�Ĝ@�Z@�1'@��@�o@���@���@�Q�@���@���@���@��9@�\)@�/@�l�@�
=@��H@�{@��9@�1'@��;@��P@�l�@�ȴ@�V@�x�@�I�@�
=@�=q@�X@�bN@�b@��@�"�@��y@���@�ȴ@��R@��!@��\@�~�@�n�@�V@�@���@���@�`B@�7L@���@��
@�"�@�o@��y@�E�@�?}@���@�~�@�@���@��h@��@�O�@���@�  @�^5@��h@�Ĝ@�1@��P@�dZ@�C�@�+@��@��H@�v�@�E�@���@��@�Q�@�1@���@�C�@��@���@�ff@�M�@�-@�{@��T@�G�@��/@��@��u@�bN@�(�@��@��w@�|�@�+@���@���@��!@���@��+@�v�@�M�@�{@��@��@��T@���@�p�@���@�Z@���@��@�S�@�S�@�S�@���@�5?@�@���@���@�p�@���@��9@��@���@��w@��P@���@���@�33@���@�v�@��+@��+@�{@���@���@���@���@���@��`@��u@�1'@�Z@�j@�(�@+@~@|�@{��@|j@|z�@|z�@|�/@}�-@|z�@yhs@wl�@w
=@w
=@v��@vE�@v��@w|�@w|�@w
=@v5?@u��@uO�@u`B@t�/@s�
@s33@r��@r�H@so@s33@st�@s��@s@r^5@q��@qG�@q&�@p�9@q�@p�`@p�9@p�u@p �@o��@o��@o|�@ol�@o�@n��@nff@n�y@nȴ@m@m�@l�j@lz�@lz�@lj@lZ@lI�@k�m@kt�@j��@jn�@i��@i�7@ix�@h��@h��@h�`@h�u@hb@g�w@g�@fȴ@f��@e�@ep�@e`B@e��@e�-@e�-@f$�@g+@i��@i�#@i�#@i�#@i�@i��@i�^@ihs@h�9@hbN@hQ�@h1'@hb@h  @g�;@g�w@g\)@g\)@f��@f�+@f5?@f{@f{@e��@e�@d��@d�@d�@dZ@c�
@c�F@c�@co@b~�@a��@a�@a��@a�^@a��@ax�@a%@`�9@`Q�@` �@_�P@^ȴ@^v�@^V@]��@]�@]V@\�@\j@\1@[�F@[C�@Z��@Z=q@ZJ@Y�#@Y��@YG�@X�9@X�@Xb@W�w@W|�@V�y@Vv�@U�T@U��@Up�@U`B@UO�@UV@T��@T(�@Sƨ@S�F@SS�@S@R�H@R��@R��@R��@R�!@R��@R�\@R-@Q��@Q��@Q7L@Q%@P��@P�@Pr�@PQ�@P �@O�w@O\)@O
=@N��@N$�@M�-@Mp�@M?}@L�@L�D@L�@Kƨ@K��@K�@KdZ@KC�@K"�@Ko@J�@J�H@J��@J��@J��@Jn�@J-@I��@I��@I��@I��@IX@I�@HĜ@HbN@H1'@H  @G��@F��@F��@Fff@FE�@F{@F@E�@E�T@E@E�h@Ep�@E`B@D��@DI�@D1@C�m@CS�@C@B��@B��@B~�@B^5@B-@A��@A�^@Ahs@A7L@@�`@@Q�@?�;@?�w@?|�@?�@>$�@=p�@<��@<�@;ƨ@:��@9�#@9�7@9G�@97L@9&�@8��@8Ĝ@8��@8r�@8bN@81'@8  @7�;@7��@7\)@7K�@7;d@7
=@6�y@6�+@6E�@6{@5��@5@5@5@5@5@5��@5�h@5p�@5�@5p�@5p�@5`B@4�@4�D@3��@3t�@3C�@3"�@2�H@2=q@17L@0��@0r�@01'@/�@/�@/|�@/|�@/l�@/K�@/;d@/;d@/;d@/+@/+@/�@.��@.��@.V@.{@-��@-@-��@-�@-V@,z�@,9X@,(�@,1@+�F@+��@+��@+��@+�@+C�@*�H@*n�@)��@)x�@)x�@)x�@)hs@)hs@)hs@)hs@)hs@)X@)X@)&�@(��@(��@(�u@(�@(b@'\)@&ȴ@&��@&$�@%�-@%O�@$�/@$��@$�D@$�D@$z�@$j@$j@$Z@$I�@$9X@$1@#�@#t�@#dZ@#"�@"�@"�H@"�@"�H@"��@"�!@"~�@"=q@!��@!�@!�^@!�7@!7L@ �@  �@�w@+@�+@@�-@��@��@p�@`B@?}@�j@j@I�@9X@9X@�
@��@t�@C�@33@�H@��@��@��@~�@n�@=q@�^@hs@X@G�@��@Ĝ@��@�u@bN@1'@�@�@�;@��@\)@�@v�@E�@E�@E�@5?@5?@5?@�@�-@��@�@�@p�@?}@�@�@�j@Z@��@�F@dZ@"�@�H@��@n�@-@J@J@��@��@�7@G�@%@��@r�@A�@ �@b@�;@�;@�w@l�@\)@K�@;d@+@
=@ȴ@�+@v�@E�@{@@��@/@�j@�D@j@I�@�@�F@��@��@�@S�@"�@
�@
�@
�@
�H@
��@
�!@
��@
�\@
^5@
-@
�@	��@	�@	�#@	�7@	X@	&�@	%@�`@�u@Q�@  @�;@�@K�@�y@�R@��@�+@V@$�@�@�T@�-@�@?}@/@/@V@�/@�@��@��@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��A��A��A��A��A��A��A���A��A��A���A��yA��HAʾwAɶFA��
A���A���A��A��wA���A�ƨA�  A���A��A���A��
A���A��TA�ffA�ZA��HA�VA�`BA�v�A���A�$�A���A��wA�`BA�^5A��/A��uA��A� �A��jA���A��DA�=qA��A�I�A�z�A�G�A���A�33A�A���A�K�A���A��hA�ffA���A��-A�bNA�~�A��^A��A�=qA�"�A��A�%A�=qA�5?A��-A���A��A��A��\A��7A��wA�|�A��A~�!A|�uA{�mA{`BA{�AyO�Aw�;Av�!Au�mAu�7At�uAs7LAq��ApM�Ao
=Am`BAl$�Ak�wAk`BAjz�Ah{Af��Ae`BAc�7Ab�HAbffAa��AaS�Aa�A`ȴA_|�A^��A]t�A\A�A[33AZVAYx�AXffAV��AU��AUt�ATz�ASXAQ�;AQdZAP�AP�\AO�AO33AN�AM/AL�RAL �AJȴAHȴAHAG��AF�HAFQ�AE�TAE��AE�AD�RADQ�AC��AC
=AB��AA��A@�A@I�A?dZA>��A=�#A<�uA;C�A:n�A9��A9
=A8�\A7�#A7?}A5��A4�!A3��A2ȴA1�TA0~�A/�-A.�9A-�A,��A+��A+&�A*{A(�9A'XA&ZA&A%�A$^5A#"�A"^5A"A!?}A��A(�A|�A��A�!A�\AVA �AA��A`BA��AhsAȴAM�A�;A��A�/AA��AG�A/A�AQ�A
=AE�AAt�A��A��A
=A �A�A\)A
ĜA	�mA	%A�mA�A��AjA  A�hA/A�A�AZA�TA �A��A��A�7@�v�@��@�"�@�=q@��@��h@�V@�r�@�ȴ@���@�@�h@�+@�+@�5?@���@��`@�P@�ȴ@�5?@���@�ff@�Z@�~�@�A�@�dZ@�ff@ݙ�@ܛ�@�j@��@�+@�ȴ@أ�@�o@պ^@��@�  @�dZ@Ѳ-@ύP@�5?@�hs@�r�@��@�|�@�V@���@�p�@���@�$�@�Ĝ@Ý�@�"�@�ȴ@�ff@�J@��7@��@��@�A�@��P@���@��+@�ff@��@�`B@�?}@�Ĝ@�Z@�1'@��@�o@���@���@�Q�@���@���@���@��9@�\)@�/@�l�@�
=@��H@�{@��9@�1'@��;@��P@�l�@�ȴ@�V@�x�@�I�@�
=@�=q@�X@�bN@�b@��@�"�@��y@���@�ȴ@��R@��!@��\@�~�@�n�@�V@�@���@���@�`B@�7L@���@��
@�"�@�o@��y@�E�@�?}@���@�~�@�@���@��h@��@�O�@���@�  @�^5@��h@�Ĝ@�1@��P@�dZ@�C�@�+@��@��H@�v�@�E�@���@��@�Q�@�1@���@�C�@��@���@�ff@�M�@�-@�{@��T@�G�@��/@��@��u@�bN@�(�@��@��w@�|�@�+@���@���@��!@���@��+@�v�@�M�@�{@��@��@��T@���@�p�@���@�Z@���@��@�S�@�S�@�S�@���@�5?@�@���@���@�p�@���@��9@��@���@��w@��P@���@���@�33@���@�v�@��+@��+@�{@���@���@���@���@���@��`@��u@�1'@�Z@�j@�(�@+@~@|�@{��@|j@|z�@|z�@|�/@}�-@|z�@yhs@wl�@w
=@w
=@v��@vE�@v��@w|�@w|�@w
=@v5?@u��@uO�@u`B@t�/@s�
@s33@r��@r�H@so@s33@st�@s��@s@r^5@q��@qG�@q&�@p�9@q�@p�`@p�9@p�u@p �@o��@o��@o|�@ol�@o�@n��@nff@n�y@nȴ@m@m�@l�j@lz�@lz�@lj@lZ@lI�@k�m@kt�@j��@jn�@i��@i�7@ix�@h��@h��@h�`@h�u@hb@g�w@g�@fȴ@f��@e�@ep�@e`B@e��@e�-@e�-@f$�@g+@i��@i�#@i�#@i�#@i�@i��@i�^@ihs@h�9@hbN@hQ�@h1'@hb@h  @g�;@g�w@g\)@g\)@f��@f�+@f5?@f{@f{@e��@e�@d��@d�@d�@dZ@c�
@c�F@c�@co@b~�@a��@a�@a��@a�^@a��@ax�@a%@`�9@`Q�@` �@_�P@^ȴ@^v�@^V@]��@]�@]V@\�@\j@\1@[�F@[C�@Z��@Z=q@ZJ@Y�#@Y��@YG�@X�9@X�@Xb@W�w@W|�@V�y@Vv�@U�T@U��@Up�@U`B@UO�@UV@T��@T(�@Sƨ@S�F@SS�@S@R�H@R��@R��@R��@R�!@R��@R�\@R-@Q��@Q��@Q7L@Q%@P��@P�@Pr�@PQ�@P �@O�w@O\)@O
=@N��@N$�@M�-@Mp�@M?}@L�@L�D@L�@Kƨ@K��@K�@KdZ@KC�@K"�@Ko@J�@J�H@J��@J��@J��@Jn�@J-@I��@I��@I��@I��@IX@I�@HĜ@HbN@H1'@H  @G��@F��@F��@Fff@FE�@F{@F@E�@E�T@E@E�h@Ep�@E`B@D��@DI�@D1@C�m@CS�@C@B��@B��@B~�@B^5@B-@A��@A�^@Ahs@A7L@@�`@@Q�@?�;@?�w@?|�@?�@>$�@=p�@<��@<�@;ƨ@:��@9�#@9�7@9G�@97L@9&�@8��@8Ĝ@8��@8r�@8bN@81'@8  @7�;@7��@7\)@7K�@7;d@7
=@6�y@6�+@6E�@6{@5��@5@5@5@5@5@5��@5�h@5p�@5�@5p�@5p�@5`B@4�@4�D@3��@3t�@3C�@3"�@2�H@2=q@17L@0��@0r�@01'@/�@/�@/|�@/|�@/l�@/K�@/;d@/;d@/;d@/+@/+@/�@.��@.��@.V@.{@-��@-@-��@-�@-V@,z�@,9X@,(�@,1@+�F@+��@+��@+��@+�@+C�@*�H@*n�@)��@)x�@)x�@)x�@)hs@)hs@)hs@)hs@)hs@)X@)X@)&�@(��@(��@(�u@(�@(b@'\)@&ȴ@&��@&$�@%�-@%O�@$�/@$��@$�D@$�D@$z�@$j@$j@$Z@$I�@$9X@$1@#�@#t�@#dZ@#"�@"�@"�H@"�@"�H@"��@"�!@"~�@"=q@!��@!�@!�^@!�7@!7L@ �@  �@�w@+@�+@@�-@��@��@p�@`B@?}@�j@j@I�@9X@9X@�
@��@t�@C�@33@�H@��@��@��@~�@n�@=q@�^@hs@X@G�@��@Ĝ@��@�u@bN@1'@�@�@�;@��@\)@�@v�@E�@E�@E�@5?@5?@5?@�@�-@��@�@�@p�@?}@�@�@�j@Z@��@�F@dZ@"�@�H@��@n�@-@J@J@��@��@�7@G�@%@��@r�@A�@ �@b@�;@�;@�w@l�@\)@K�@;d@+@
=@ȴ@�+@v�@E�@{@@��@/@�j@�D@j@I�@�@�F@��@��@�@S�@"�@
�@
�@
�@
�H@
��@
�!@
��@
�\@
^5@
-@
�@	��@	�@	�#@	�7@	X@	&�@	%@�`@�u@Q�@  @�;@�@K�@�y@�R@��@�+@V@$�@�@�T@�-@�@?}@/@/@V@�/@�@��@��@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111BD�BD�BD�BD�BD�BE�BD�BD�BE�BE�BE�BF�BF�BH�B[#B�VB��B��B��B��B�uBx�BgmB^5BO�B33B(�BPB�B�B  B��B�`B�B��BƨB�dB�-B��B��B�Bx�Bt�Bp�BgmBYBJ�BF�BA�B=qB49B)�B�B\B
=BBB��B�B�yB�
B��B�qB��B�Bw�BjB_;BM�B<jB(�B�B1B
��B
��B
��B
�B
�`B
��B
ǮB
�FB
�B
��B
�1B
�B
�B
~�B
r�B
gmB
`BB
YB
VB
O�B
F�B
<jB
2-B
(�B
�B
oB
\B
DB
+B	��B	�B	�`B	�B	��B	��B	��B	ɺB	ǮB	ƨB	��B	�qB	�?B	�!B	��B	��B	��B	��B	�JB	�B	�B	~�B	z�B	t�B	p�B	o�B	m�B	jB	dZB	`BB	ZB	T�B	P�B	K�B	?}B	:^B	8RB	5?B	49B	33B	2-B	1'B	-B	(�B	%�B	 �B	�B	�B	{B	bB	bB	PB		7B	B��B��B��B��B��B�B�B�B�fB�;B�B�
B��B��BǮBĜBB�jB�XB�FB�!B�B��B��B��B��B��B�uB�oB�bB�DB�1B�B�B�B� B� B~�B}�B|�B{�By�By�Bv�Bt�Bs�Br�Bq�Bo�Bm�Bl�Bl�Bk�BjBe`BcTBbNBaHB`BB^5B\)BYBXBW
BS�BQ�BP�BM�BK�BI�BI�BI�BG�BE�BC�BA�B@�B@�B?}B?}B>wB@�BB�B=qB5?B1'B/B.B-B,B,B-B-B.B)�B(�B'�B'�B'�B%�B%�B#�B!�B$�B"�B#�B �B�B�B �B �B!�B!�B!�B �B"�B"�B#�B$�B%�B$�B&�B'�B(�B)�B)�B+B+B-B,B,B,B-B.B/B0!B0!B0!B0!B0!B0!B1'B1'B2-B2-B33B33B49B5?B5?B6FB6FB6FB6FB8RB7LB9XB:^B:^B9XB;dB?}B?}BD�BD�BG�BH�BJ�BL�BN�BP�BP�BP�BS�BVBXB\)B`BBcTBffBiyBjBk�Bm�Bn�Bn�Bn�Bn�Bo�Bo�Bo�Bo�Bp�Bq�Br�Bs�Bs�Bt�Bv�By�B|�B|�B|�B� B�B�7B�VB�bB�hB�hB�hB�oB�{B��B��B��B��B��B�B�B�B�B�B�B�'B�'B�3B�^B�dB�qB�wB��BĜBǮBɺB��B��B��B��B��B��B��B�B�
B�B�B�#B�/B�;B�BB�HB�NB�TB�TB�ZB�`B�mB�sB�mB�sB�sB�B�B�B��B��B��B��B��B��B	  B	B	B	B	1B		7B	
=B	DB	hB	uB	�B	�B	�B	�B	�B	�B	�B	!�B	#�B	%�B	&�B	(�B	,B	0!B	33B	5?B	8RB	<jB	>wB	@�B	?}B	?}B	>wB	@�B	G�B	I�B	I�B	K�B	O�B	R�B	S�B	VB	XB	\)B	^5B	`BB	dZB	hsB	iyB	jB	jB	l�B	n�B	p�B	r�B	r�B	t�B	u�B	v�B	y�B	z�B	z�B	|�B	}�B	}�B	}�B	}�B	~�B	�B	�B	�B	�%B	�%B	�+B	�7B	�=B	�DB	�DB	�JB	�VB	�\B	�bB	�oB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�3B	�3B	�9B	�?B	�FB	�LB	�XB	�jB	ƨB	ȴB	ɺB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�B	�#B	�#B	�)B	�/B	�5B	�;B	�BB	�HB	�TB	�ZB	�ZB	�`B	�`B	�`B	�fB	�mB	�sB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
+B
+B
1B
1B
	7B
	7B

=B

=B

=B

=B
DB
JB
JB
PB
VB
\B
\B
\B
bB
bB
hB
hB
hB
oB
oB
oB
oB
uB
uB
uB
uB
uB
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
"�B
"�B
"�B
"�B
#�B
#�B
#�B
$�B
%�B
%�B
%�B
&�B
&�B
'�B
(�B
)�B
+B
+B
-B
/B
/B
/B
/B
/B
0!B
0!B
0!B
0!B
0!B
1'B
1'B
1'B
2-B
2-B
2-B
2-B
33B
33B
33B
49B
49B
49B
49B
49B
49B
49B
49B
49B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
6FB
7LB
7LB
7LB
7LB
7LB
8RB
:^B
:^B
:^B
;dB
;dB
;dB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
=qB
=qB
=qB
>wB
>wB
>wB
>wB
>wB
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
C�B
C�B
C�B
C�B
D�B
C�B
D�B
D�B
D�B
D�B
E�B
E�B
F�B
G�B
G�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
K�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
L�B
M�B
M�B
M�B
N�B
N�B
O�B
O�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
T�B
T�B
VB
VB
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
YB
YB
YB
YB
YB
YB
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
\)B
\)B
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
]/B
]/B
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
`BB
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
cTB
cTB
cTB
cTB
dZB
dZB
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
jB
jB
jB
jB
jB
jB
k�B
l�B
l�B
l�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111BD�BD�BD�BD�BD�BE�BD�BD�BE�BE�BE�BGBG�BLdB`�B��B�=B��B�B�bB��Bz�Bh�BaBTaB8B/ B}B�B�B{B��B�B�B�\B��B�BB�%B�FB�)B�ABy�BvFBs3BjKBZ�BK�BGzBB�B>�B6FB,qB	BbB)B�BB��B��B�B�KB͹B�'B��B�By�BlqBa�BPbB>�B+B�B	7B
�PB
�xB
�	B
�AB
��B
�?B
�rB
�lB
�/B
��B
�7B
��B
��B
��B
tnB
h�B
aHB
Y�B
WsB
Q�B
H�B
>(B
3�B
*�B
!B
B
.B
�B
	�B	��B	�cB	�8B	��B	��B	ЗB	�jB	�=B	ȀB	�1B	��B	��B	��B	�vB	�*B	�,B	�BB	�kB	�jB	�B	�gB	��B	|jB	uZB	qAB	p;B	n}B	k�B	ezB	a�B	Z�B	VB	R�B	M�B	@�B	;B	9>B	5�B	4�B	3�B	2�B	1�B	-�B	)�B	&�B	!�B	�B	�B	�B	�B	�B	�B	
�B	�B��B��B��B��B��B��B�AB�"B�B�BۦB��B� B�<B�B�%B�B��B��B�8B��B�CB��B��B�ZB�;B��B�aB��B��B�B�B��B��B�UB�iB�iBHB~wB}�B|�B{�Bz�Bw�ButBtnBs�Br�Bp;BnBl�Bm)Bl�BlBffBd@BcBb�BaHB_�B]dBZ�BZ�BXBUMBS@BRoBN�BLJBJrBJrBJXBHKBF%BDBB'BA�BBuB@ B?�B?�BC{BD�B?}B5�B1�B/�B.�B-�B-B,�B-]B-�B/�B*B)yB(sB(�B(�B&�B&�B$�B$B&LB$@B%,B!|B �B vB!bB!B"NB"hB"�B"NB$B#�B$tB%�B&�B&2B(XB(�B)�B*�B*�B+�B+�B-�B,�B-CB-CB.B.�B/�B0�B0�B0�B0�B0�B0�B1�B1�B2�B2�B3hB3�B4�B5tB5�B6�B6�B6�B6�B8�B8B:DB:�B:�B:^B<�B@�BABE�BEBG�BIlBK�BM6BO(BQNBQNBQ�BT{BV�BX�B]B`�Bd&BgBi�Bj�Bk�Bm�Bn�Bn�Bn�Bn�Bo�Bo�Bo�Bo�Bp�Bq�Br�BtBtBu?BwfBzxB}<B}<B}�B�B�B�#B��B��B��B��B��B�B�2B��B�VB�hB�fB�KB�=B�CB�/B�IB��B��B��B��B�B��B��B��B��B��B��B��B��B��B��B�B�PB�:B�,B�MB�9B�YB�EB�QB�qBݘB�pB�vB�bB�hB�nB�nB�B�B�B�B�B�B��B�=B��B�B��B�B��B�$B�xB�BB	 iB	'B	AB	{B	�B		lB	
rB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	"4B	#�B	%�B	'B	)DB	,WB	0�B	3�B	5�B	8RB	<�B	>�B	AB	@ B	?�B	>�B	@�B	G�B	I�B	I�B	K�B	P}B	TB	T�B	VSB	X+B	\]B	^jB	`'B	d@B	h�B	i�B	j�B	j�B	l�B	n�B	qB	r�B	r�B	t�B	u�B	v�B	y�B	z�B	{B	}VB	~BB	~(B	~(B	~B	B	�B	�3B	�9B	�YB	�YB	�EB	�RB	�XB	�^B	�xB	�~B	�VB	�\B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	��B	��B	��B	�B	��B	�B	�B	�$B	�0B	�KB	�6B	�CB	�vB	�MB	�3B	�9B	�?B	�FB	�B	��B	�B	ƎB	ȴB	ɺB	ɺB	��B	��B	�B	�B	�B	��B	�B	��B	��B	��B	� B	� B	�B	�FB	�MB	�$B	�+B	�KB	�1B	�KB	�QB	�WB	�=B	�]B	�dB	�OB	�pB	�vB	�|B	�B	�tB	�B	�zB	�zB	�zB	�B	�B	�B	�B	�B	��B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�	B	�B	�B	�0B	�B	�"B	�"B	�BB	�.B
 4B
 B
 B
 B
;B
'B
[B
-B
-B
GB
GB
3B
9B
B
B
?B
?B
?B
tB
_B
zB
fB
KB
	RB
	RB

XB

XB

XB

rB
xB
dB
~B
�B
�B
�B
vB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
uB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
 B
 �B
!�B
!�B
!�B
"�B
"�B
"�B
"�B
$B
#�B
$B
%B
%�B
%�B
&B
'B
'RB
(>B
)DB
*KB
+QB
+kB
-wB
/5B
/OB
/B
/5B
/5B
0;B
0;B
0;B
0;B
0UB
1AB
1AB
1AB
2GB
2GB
2GB
2GB
3MB
3MB
3hB
4TB
4TB
4TB
49B
4TB
49B
49B
4TB
49B
5?B
5?B
5?B
5?B
5tB
5tB
5tB
6zB
7�B
7fB
7fB
7�B
7�B
8�B
:�B
:xB
:xB
;B
;�B
;B
<jB
<�B
<�B
<jB
<jB
<�B
<jB
<jB
<�B
<�B
<�B
=�B
=�B
=�B
>wB
>�B
>�B
>�B
>�B
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
C�B
C�B
C�B
C�B
D�B
C�B
D�B
D�B
D�B
D�B
E�B
E�B
F�B
G�B
G�B
IB
IB
H�B
I�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
KB
K�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
MB
L�B
L�B
MB
M�B
NB
N"B
O(B
O(B
P.B
PB
QB
RB
Q�B
Q�B
RB
RB
R B
R B
SB
SB
R�B
S&B
S&B
T,B
TB
TB
T,B
TB
TB
T�B
UB
U2B
UB
U2B
UMB
VB
VB
VB
V9B
VB
W$B
W$B
W?B
W?B
W?B
W
B
W$B
W$B
W?B
XEB
XEB
YKB
Y1B
Y1B
YB
YB
YB
X+B
YKB
YB
Y1B
YB
Y1B
YKB
YKB
Y1B
ZQB
ZkB
Z7B
Z7B
[WB
\CB
\]B
\CB
\CB
\CB
\CB
\)B
\CB
]dB
]IB
]IB
]IB
]dB
]dB
^jB
^OB
^5B
_pB
_VB
_VB
_VB
_;B
_VB
`BB
`\B
`vB
`\B
`vB
`\B
`\B
`\B
`\B
abB
a�B
a�B
bhB
bhB
b�B
bhB
bhB
cTB
cTB
cnB
cnB
cnB
cnB
cTB
cTB
cTB
cTB
cnB
dZB
dtB
d�B
d�B
dtB
dtB
dtB
dtB
dtB
ezB
ezB
ezB
e�B
ezB
f�B
f�B
f�B
g�B
g�B
h�B
h�B
h�B
h�B
h�B
h�B
i�B
i�B
i�B
i�B
j�B
jB
jB
j�B
j�B
j�B
k�B
l�B
l�B
l�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.05(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201607240037202016072400372020160724003720201806221211162018062212111620180622121116201804050403382018040504033820180405040338  JA  ARFMdecpA19c                                                                20160720093504  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20160720003514  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20160720003514  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20160720003514  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20160720003515  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20160720003515  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20160720003515  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20160720003515  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20160720003515  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20160720003516                      G�O�G�O�G�O�                JA  ARUP                                                                        20160720012023                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20160720153613  CV  JULD            G�O�G�O�F��                JM  ARGQJMQC2.0                                                                 20160720153613  CV  JULD_LOCATION   G�O�G�O�F��                JM  ARGQJMQC2.0                                                                 20160720153613  CV  LATITUDE        G�O�G�O�A��                JM  ARGQJMQC2.0                                                                 20160720153613  CV  LONGITUDE       G�O�G�O��#�                JM  ARCAJMQC2.0                                                                 20160723153720  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20160723153720  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404190338  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622031116  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115101518                      G�O�G�O�G�O�                