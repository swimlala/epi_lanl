CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2019-04-16T09:36:47Z creation;2019-04-16T09:36:50Z conversion to V3.1;2019-12-23T06:03:44Z update;     
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
resolution        =���   axis      Z        t  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     t  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \d   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     t  `D   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     t  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     t  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �`   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     t  �@   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     t  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     t  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     t  �|   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  `  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �P   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �P   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �P   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T  �P   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �    HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �$Argo profile    3.1 1.2 19500101000000  20190416093647  20200120031517  5905058                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0675_139                     2C  D   NAVIS_A                         0675                            ARGO 102115                     863 @ض�Er�1   @ض��[ @8ٓ����c%�>BZ�1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @@  @�  @���A   A   A@  A`  A�  A�  A�33A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ Dݼ�D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�9�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @K�@�@\A�HA"�HAB�HAb�HA�p�A�p�A���A�p�A�p�A�p�A�p�A�p�B �RB�RB�RB�RB �RB(�RB0�RB8�RB@�RBH�RBP�RBX�RB`�RBh�RBp�RBx�RB�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)C .C.C.C.C.C
.C.C.C.C.C.C.C.C.C.C.C .C".C$.C&.C(.C*.C,.C..C0.C2.C4.C6.C8.C:.C<.C>.C@.CB.CD.CF.CH.CJ.CL.CN.CP.CR.CT.CV.CX.CZ.C\.C^.C`.Cb.Cd.Cf.Ch.Cj.Cl.Cn.Cp.Cr.Ct.Cv.Cx.Cz.C|.C~.C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�#�C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�#�C�#�C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�#�C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw��Dx�Dx��Dy�Dy��Dz�Dz��D{�D{��D|�D|��D}�D}��D~�D~��D�D��D��D�E�D���D���D��D�E�D���D���D��D�E�D���D���D��D�E�D���D���D��D�E�D���D���D��D�E�D���D���D��D�E�D���D���D��D�E�D���D���D��D�E�D���D���D��D�E�D���D���D��D�E�D���D���D��D�E�D���D���D��D�E�D���D���D��D�E�D���D���D��D�E�D���D���D��D�E�D���D���D��D�E�D���D���D��D�E�D���D���D��D�E�D���D���D��D�E�D���D���D��D�E�D���D���D��D�E�D���D���D��D�E�D���D���D��D�E�D���D���D��D�E�D���D���D��D�E�D���D���D��D�E�D���D���D��D�E�D���D���D��D�E�D���D���D��D�E�D���D���D��D�E�D���D���D��D�E�D���D���D��D�E�D���D���D��D�E�D���D���D��D�E�D���D���D��D�E�D���D���D��D�E�D���D���D��D�E�D���D���D��D�E�D���D���D��D�E�D���D���D��D�E�D���D���D��D�E�D���D���D��D�E�D���D���D��D�E�D���D���D��D�E�D���D���D��D�E�D���D���D��D�E�D���D���D��D�E�D���D���D��D�E�D���D���D��D�E�D���D���D��D�E�D���D���D��D�E�D���D���D��D�E�D���D���D��D�E�D���D���D��D�E�D���D���D��D�E�D���D���D��D�E�D���D���D��D�E�D���D���D��D�E�D���D���D��D�E�D���D���D��D�E�D���D���D��D�E�D���D���D��D�E�D���D���D��D�E�D���D���D��D�E�D���D���D��D�E�D���D���D��D�E�D�D���D��D�E�DÅ�D���D��D�E�Dą�D���D��D�E�DŅ�D���D��D�E�Dƅ�D���D��D�E�Dǅ�D���D��D�E�Dȅ�D���D��D�E�DɅ�D���D��D�E�Dʅ�D���D��D�E�D˅�D���D��D�E�D̅�D���D��D�E�Dͅ�D���D��D�E�D΅�D���D��D�E�Dυ�D���D��D�E�DЅ�D���D��D�E�Dх�D���D��D�E�D҅�D���D��D�E�DӅ�D���D��D�E�Dԅ�D���D��D�E�DՅ�D���D��D�E�Dօ�D���D��D�E�Dׅ�D���D��D�E�D؅�D���D��D�E�Dم�D���D��D�E�Dڅ�D���D��D�E�Dۅ�D���D��D�E�D܅�D���D��D�E�D݅�D�D��D�E�Dޅ�D���D��D�E�D߅�D���D��D�E�D���D���D��D�E�D��D���D��D�E�D��D���D��D�E�D��D���D��D�E�D��D���D��D�E�D��D���D��D�E�D��D���D��D�E�D��D���D��D�E�D��D���D��D�E�D��D���D��D�E�D��D���D��D�E�D��D���D��D�E�D��D���D��D�E�D��D���D��D�E�D��D���D��D�E�D��D���D��D�E�D���D���D��D�E�D��D���D��D�E�D��D���D��D�E�D��D���D��D�E�D��D���D��D�E�D���D���D��D�E�D���D���D��D�?\11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�7LA�5?A�1'A�(�A�$�A�/A�5?A�9XA�7LA�9XA�=qA�?}A�=qA�7LA�;dA�5?A�33A�7LA�5?A�7LA�1'A�JA�%A�ȴA�~�A�E�A��A�ffA���A���A�O�A���A�`BA�7LA���A�dZA�oA�?}A��A�oA�VA��A���A�&�A�E�A�~�A�%A��/A��/A���A�jA��;A��mA��A�|�A��A��A�ƨA��uA�l�A�VA��PA�1A��9A�K�A��A�bNA�A�A�{A�ȴA�jA�%A��A�n�A��A�7LA�ȴA��hA��A�XA�$�A�G�A��9A�|�A�A�jA��A���A��uA�/A�oA���A�-A��HA�t�A��A�1'A��\A�bNA�|�A�ĜA�-A��7A�ĜA��hA��jA���A�dZA�A�XA|��A{S�Ax��Ax�AvjAt~�An�Ah�uAe+Ac/A_��A]l�A[/AY;dAX��AX��AXȴAV�/AR��AP  AN5?ALn�AK�;AI�#AH(�AG�7AFz�AB�AA��A@�HA@-A?�^A?+A=dZA;�
A9��A8�DA8  A7l�A57LA3�A1G�A/�A/oA.ffA.A-�PA,��A*�+A)��A(��A'��A&A�A$��A"n�A!&�A!VA 1'At�A�A��AA��A�jAA�7A��A  A��A�
A9XA�A=qA�A��A��AVAjA�Ax�A��AVA �A�
A��A�A�A%A
��A	��A	K�A9XA��AA�A
=A�A ��@��@�v�@��j@�-@�r�@���@�E�@�ƨ@�+@�^5@�?}@�9@�1@�"�@��H@�^5@��`@��
@��H@ꟾ@�J@�G�@�D@�l�@�5?@�@�/@�D@��@�`B@���@�r�@�  @ߕ�@���@���@��/@�l�@ڸR@��#@��/@�dZ@��@��@�E�@և+@ְ!@֟�@�~�@�^5@�$�@���@�|�@�^5@��;@�{@̼j@�o@�J@�A�@��
@ǅ@�"�@�{@�O�@�z�@�|�@�n�@���@��7@�X@�(�@�ff@�@��^@�G�@���@�9X@�l�@��@���@���@�I�@�dZ@���@��!@���@�&�@���@���@��@�C�@��@�@��R@���@��@��u@�1'@��
@��w@���@��@�o@���@���@��@�I�@��@�ȴ@�5?@�`B@��9@�I�@�ƨ@�M�@��@�A�@�t�@�o@���@���@�G�@�V@��u@��w@���@�|�@�K�@�33@�o@�
=@��@���@��\@�n�@��T@��@�p�@�`B@��@���@���@���@�z�@�Q�@�Q�@�I�@�9X@�b@���@�\)@�;d@�33@��@�@���@�@�@��@�J@���@�$�@���@�x�@�X@�p�@��h@��@���@�&�@�z�@�A�@� �@���@���@�t�@�S�@�"�@��@�o@��@��R@���@�v�@�@��T@���@�x�@��@���@�Q�@���@�C�@�"�@�@���@�5?@���@�x�@��`@���@��@�bN@��
@���@��@��P@��P@�"�@��@��\@�V@��@��T@���@�x�@�O�@�?}@��@��D@�I�@�A�@�A�@�A�@�9X@�(�@���@��
@�ƨ@��F@���@��@�|�@�S�@�
=@�
=@���@��\@���@�v�@�ff@�E�@���@�@���@��@�G�@��@��@��/@���@��9@���@��@�Z@�9X@�1@���@��@��
@��w@��@���@��@�l�@�;d@��y@���@���@���@�n�@�$�@��@�@���@���@��h@�x�@�/@���@��9@�z�@�Z@��@�  @��@��w@��P@�|�@�\)@�C�@��@�o@�@��H@���@��\@�n�@�5?@�@�@��h@�x�@�p�@�G�@�&�@�%@��@��9@�z�@�(�@�@;d@~�@~V@}`B@}�@|�@|Z@|�@|1@{ƨ@{t�@z��@z=q@y�@y��@yhs@yG�@xĜ@x1'@w�;@w�P@v�y@vȴ@v�+@u�-@t��@t�D@t1@sƨ@s��@s��@st�@sS�@r�@r�!@r~�@r�@q��@qhs@q�@p�u@o�w@ol�@o\)@o�@n{@m�-@m�-@m`B@l��@l�/@l�D@k�F@kC�@j�\@jJ@i��@i7L@hr�@g�P@f��@ep�@e/@d��@d�/@d�@d�D@d1@c��@b�H@b^5@a�^@`�`@`�@`1'@_��@_l�@_+@^�@^5?@]��@]�@\��@\��@\Z@\�@[�@[33@Z�@Z�!@Z=q@Y�@Yhs@Y7L@X��@X�9@XbN@X1'@X  @W�@W�w@W�P@W�P@W\)@V��@VE�@U�@U/@T�j@Tj@T(�@S�m@S�@R�\@R-@R�@RJ@Q�#@Q�7@Qhs@PĜ@P�@PbN@PA�@O�;@O��@O\)@O+@N��@Nȴ@N�+@NV@N5?@N5?@N$�@N{@M�@M�-@M�@MO�@M�@L��@L�D@L(�@K�m@Kƨ@K�F@Kt�@Ko@J�!@J^5@J�@I�#@I�7@I�@H��@H�u@HbN@H1'@Hb@G�@G�@G��@G�P@Gl�@G
=@F�@F�+@F{@E��@E��@E�@E`B@D�/@Dz�@DI�@D�@C�m@Cƨ@C��@C�@Ct�@CdZ@C"�@B�@B~�@A�@A��@Ax�@@Ĝ@@r�@@1'@@  @?�;@?�P@?|�@?l�@?l�@?+@>��@>��@>ȴ@>ff@>{@=�-@=p�@=/@<��@<�/@<�j@<��@<z�@<Z@<(�@;��@;ƨ@;�F@;��@;S�@;"�@;o@;@:��@:��@:�\@:^5@9��@9��@9�^@9��@9�7@9X@9G�@9&�@9�@9%@8Ĝ@8�9@8 �@8  @7�;@7�@7;d@6�@6�R@6�+@6v�@6ff@65?@5�@5`B@5�@4��@4Z@49X@4(�@3�
@3t�@3"�@2�!@2n�@2J@1�^@1X@1%@0�u@0bN@01'@/�@/;d@.�y@.�+@.V@.$�@-�T@-�-@-�@-/@,�j@,Z@,I�@,9X@,�@+��@+��@+dZ@+C�@*�H@*-@)�#@)x�@)7L@)�@(��@(�`@(�u@(�u@(�@(bN@(b@'�@'�w@'�P@'l�@'�@&ȴ@&�R@&��@&ff@&@%@%p�@%O�@%?}@%�@$�j@$j@$Z@$I�@$I�@$I�@$I�@$9X@$(�@$1@#�m@#ƨ@#��@#t�@#S�@#"�@"�H@"�!@"�\@"~�@"M�@"�@!�#@!hs@ �`@ ��@ r�@ Q�@ A�@ 1'@  �@�;@�w@�@�P@K�@�@�y@ȴ@��@v�@�@��@`B@O�@/@V@�/@�@z�@I�@1@�
@��@dZ@�@�\@-@J@�#@��@�7@x�@x�@hs@X@%@��@��@r�@b@��@;d@�@��@�+@ff@E�@5?@@��@�-@�h@`B@?}@/@V@�@��@��@j@I�@9X@(�@��@�F@C�@@��@~�@n�@=q@�@�#@�^@hs@�@%@��@�u@�@bN@1'@�@��@�@�P@l�@K�@;d@
=@�@��@v�@V@{@@�@��@�@V@�j@�D@z�@I�@9X@(�@�@�F@�@dZ@
�@
��@
n�@
M�@
=q@
-@
-@
�@	�@	�^@	�7@	&�@	%11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�7LA�5?A�1'A�(�A�$�A�/A�5?A�9XA�7LA�9XA�=qA�?}A�=qA�7LA�;dA�5?A�33A�7LA�5?A�7LA�1'A�JA�%A�ȴA�~�A�E�A��A�ffA���A���A�O�A���A�`BA�7LA���A�dZA�oA�?}A��A�oA�VA��A���A�&�A�E�A�~�A�%A��/A��/A���A�jA��;A��mA��A�|�A��A��A�ƨA��uA�l�A�VA��PA�1A��9A�K�A��A�bNA�A�A�{A�ȴA�jA�%A��A�n�A��A�7LA�ȴA��hA��A�XA�$�A�G�A��9A�|�A�A�jA��A���A��uA�/A�oA���A�-A��HA�t�A��A�1'A��\A�bNA�|�A�ĜA�-A��7A�ĜA��hA��jA���A�dZA�A�XA|��A{S�Ax��Ax�AvjAt~�An�Ah�uAe+Ac/A_��A]l�A[/AY;dAX��AX��AXȴAV�/AR��AP  AN5?ALn�AK�;AI�#AH(�AG�7AFz�AB�AA��A@�HA@-A?�^A?+A=dZA;�
A9��A8�DA8  A7l�A57LA3�A1G�A/�A/oA.ffA.A-�PA,��A*�+A)��A(��A'��A&A�A$��A"n�A!&�A!VA 1'At�A�A��AA��A�jAA�7A��A  A��A�
A9XA�A=qA�A��A��AVAjA�Ax�A��AVA �A�
A��A�A�A%A
��A	��A	K�A9XA��AA�A
=A�A ��@��@�v�@��j@�-@�r�@���@�E�@�ƨ@�+@�^5@�?}@�9@�1@�"�@��H@�^5@��`@��
@��H@ꟾ@�J@�G�@�D@�l�@�5?@�@�/@�D@��@�`B@���@�r�@�  @ߕ�@���@���@��/@�l�@ڸR@��#@��/@�dZ@��@��@�E�@և+@ְ!@֟�@�~�@�^5@�$�@���@�|�@�^5@��;@�{@̼j@�o@�J@�A�@��
@ǅ@�"�@�{@�O�@�z�@�|�@�n�@���@��7@�X@�(�@�ff@�@��^@�G�@���@�9X@�l�@��@���@���@�I�@�dZ@���@��!@���@�&�@���@���@��@�C�@��@�@��R@���@��@��u@�1'@��
@��w@���@��@�o@���@���@��@�I�@��@�ȴ@�5?@�`B@��9@�I�@�ƨ@�M�@��@�A�@�t�@�o@���@���@�G�@�V@��u@��w@���@�|�@�K�@�33@�o@�
=@��@���@��\@�n�@��T@��@�p�@�`B@��@���@���@���@�z�@�Q�@�Q�@�I�@�9X@�b@���@�\)@�;d@�33@��@�@���@�@�@��@�J@���@�$�@���@�x�@�X@�p�@��h@��@���@�&�@�z�@�A�@� �@���@���@�t�@�S�@�"�@��@�o@��@��R@���@�v�@�@��T@���@�x�@��@���@�Q�@���@�C�@�"�@�@���@�5?@���@�x�@��`@���@��@�bN@��
@���@��@��P@��P@�"�@��@��\@�V@��@��T@���@�x�@�O�@�?}@��@��D@�I�@�A�@�A�@�A�@�9X@�(�@���@��
@�ƨ@��F@���@��@�|�@�S�@�
=@�
=@���@��\@���@�v�@�ff@�E�@���@�@���@��@�G�@��@��@��/@���@��9@���@��@�Z@�9X@�1@���@��@��
@��w@��@���@��@�l�@�;d@��y@���@���@���@�n�@�$�@��@�@���@���@��h@�x�@�/@���@��9@�z�@�Z@��@�  @��@��w@��P@�|�@�\)@�C�@��@�o@�@��H@���@��\@�n�@�5?@�@�@��h@�x�@�p�@�G�@�&�@�%@��@��9@�z�@�(�@�@;d@~�@~V@}`B@}�@|�@|Z@|�@|1@{ƨ@{t�@z��@z=q@y�@y��@yhs@yG�@xĜ@x1'@w�;@w�P@v�y@vȴ@v�+@u�-@t��@t�D@t1@sƨ@s��@s��@st�@sS�@r�@r�!@r~�@r�@q��@qhs@q�@p�u@o�w@ol�@o\)@o�@n{@m�-@m�-@m`B@l��@l�/@l�D@k�F@kC�@j�\@jJ@i��@i7L@hr�@g�P@f��@ep�@e/@d��@d�/@d�@d�D@d1@c��@b�H@b^5@a�^@`�`@`�@`1'@_��@_l�@_+@^�@^5?@]��@]�@\��@\��@\Z@\�@[�@[33@Z�@Z�!@Z=q@Y�@Yhs@Y7L@X��@X�9@XbN@X1'@X  @W�@W�w@W�P@W�P@W\)@V��@VE�@U�@U/@T�j@Tj@T(�@S�m@S�@R�\@R-@R�@RJ@Q�#@Q�7@Qhs@PĜ@P�@PbN@PA�@O�;@O��@O\)@O+@N��@Nȴ@N�+@NV@N5?@N5?@N$�@N{@M�@M�-@M�@MO�@M�@L��@L�D@L(�@K�m@Kƨ@K�F@Kt�@Ko@J�!@J^5@J�@I�#@I�7@I�@H��@H�u@HbN@H1'@Hb@G�@G�@G��@G�P@Gl�@G
=@F�@F�+@F{@E��@E��@E�@E`B@D�/@Dz�@DI�@D�@C�m@Cƨ@C��@C�@Ct�@CdZ@C"�@B�@B~�@A�@A��@Ax�@@Ĝ@@r�@@1'@@  @?�;@?�P@?|�@?l�@?l�@?+@>��@>��@>ȴ@>ff@>{@=�-@=p�@=/@<��@<�/@<�j@<��@<z�@<Z@<(�@;��@;ƨ@;�F@;��@;S�@;"�@;o@;@:��@:��@:�\@:^5@9��@9��@9�^@9��@9�7@9X@9G�@9&�@9�@9%@8Ĝ@8�9@8 �@8  @7�;@7�@7;d@6�@6�R@6�+@6v�@6ff@65?@5�@5`B@5�@4��@4Z@49X@4(�@3�
@3t�@3"�@2�!@2n�@2J@1�^@1X@1%@0�u@0bN@01'@/�@/;d@.�y@.�+@.V@.$�@-�T@-�-@-�@-/@,�j@,Z@,I�@,9X@,�@+��@+��@+dZ@+C�@*�H@*-@)�#@)x�@)7L@)�@(��@(�`@(�u@(�u@(�@(bN@(b@'�@'�w@'�P@'l�@'�@&ȴ@&�R@&��@&ff@&@%@%p�@%O�@%?}@%�@$�j@$j@$Z@$I�@$I�@$I�@$I�@$9X@$(�@$1@#�m@#ƨ@#��@#t�@#S�@#"�@"�H@"�!@"�\@"~�@"M�@"�@!�#@!hs@ �`@ ��@ r�@ Q�@ A�@ 1'@  �@�;@�w@�@�P@K�@�@�y@ȴ@��@v�@�@��@`B@O�@/@V@�/@�@z�@I�@1@�
@��@dZ@�@�\@-@J@�#@��@�7@x�@x�@hs@X@%@��@��@r�@b@��@;d@�@��@�+@ff@E�@5?@@��@�-@�h@`B@?}@/@V@�@��@��@j@I�@9X@(�@��@�F@C�@@��@~�@n�@=q@�@�#@�^@hs@�@%@��@�u@�@bN@1'@�@��@�@�P@l�@K�@;d@
=@�@��@v�@V@{@@�@��@�@V@�j@�D@z�@I�@9X@(�@�@�F@�@dZ@
�@
��@
n�@
M�@
=q@
-@
-@
�@	�@	�^@	�7@	&�@	%11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B
gmB
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
iyB
jB
k�B
k�B
l�B
m�B
l�B
m�B
m�B
n�B
t�B
u�B
� B
�\B
��B
ȴB
��B)�BI�Bs�B��B�^B��B��B�;B�ZB�fB�NB�BB��B�`BB�B�sB�mB�B��B	7B�B�B{�Bx�B_;BjB�B�B�B�B�!B�-B�!B�-B�9B�RB�dB�jB�jB�jB�wB�wB�jB�^B�LB�9B�-B�B�B��B��B�DB{�Bo�B\)BK�BE�B>wB5?B$�B�B�BB�B�jB�oBC�B
��B
��BB
�B
�
B
��B
ǮB
�?B
�DB
r�B
>wB
'�B
H�B
=qB
bB
VB
B
hB
  B	�sB	��B	ffB	I�B	1'B	�B	  B��B�B�yB�sB�sB�NB��B�}B�RB�B�B��B��B��B��B��B��B�oB�bB�PB�=B�%B� B�B� B|�Bz�B~�B|�By�By�Bw�Bv�Bt�Bs�Br�Br�Bq�Bs�Bn�Bk�BhsBcTB_;B^5BcTBcTBdZBe`Be`BcTBffBe`BdZBbNB`BBZB\)BW
BT�BVBR�BW
BXB[#B^5BaHBaHB`BBbNBffBiyBn�Bs�By�Bz�B}�B}�B{�Bw�BhsBaHB]/B[#BT�BO�BI�BB�B:^B7LB7LB>wB;dB:^B8RB8RB8RB;dB<jB=qB>wB?}B?}B?}B@�BA�BA�BD�BF�BH�BH�BJ�BJ�BJ�BJ�BJ�BK�BK�BK�BL�BO�BP�BS�BR�BR�BS�BVBVBYB_;BbNBcTBe`BgmBiyBk�Bp�Bq�Br�Bo�Bl�BiyBgmBffBe`Be`BffBgmBiyBhsBe`Be`BdZBffBk�Bm�Bm�Bq�Bt�Bu�Bu�Bu�Bw�B|�B�B�B�+B�1B�=B�JB�PB�\B�bB��B��B��B��B��B��B��B��B�B�B�'B�'B�'B�'B�'B�-B�-B�LB�RB�dB�wB��B��BĜBŢBƨBƨBȴBȴBȴB��BɺB��B��B��B�
B�#B�`B�sB�B�B�B�B�B�B��B��B��B	B	%B	+B	1B	JB	VB	bB	uB	�B	�B	�B	�B	�B	�B	�B	!�B	#�B	$�B	%�B	'�B	)�B	+B	0!B	2-B	49B	6FB	<jB	>wB	?}B	@�B	C�B	E�B	H�B	I�B	K�B	L�B	N�B	P�B	O�B	N�B	O�B	R�B	W
B	YB	YB	ZB	]/B	^5B	_;B	e`B	gmB	jB	o�B	n�B	o�B	p�B	s�B	u�B	w�B	{�B	{�B	|�B	�B	�B	�B	�B	�+B	�7B	�DB	�JB	�bB	�hB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�!B	�'B	�'B	�-B	�9B	�?B	�?B	�FB	�RB	�RB	�XB	�^B	�jB	�wB	�wB	�}B	��B	��B	B	B	ĜB	ŢB	ǮB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�)B	�/B	�5B	�;B	�;B	�BB	�BB	�HB	�TB	�TB	�ZB	�`B	�fB	�fB	�fB	�mB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
%B
%B
+B
+B
+B
+B
1B
1B
	7B
	7B
	7B
	7B

=B

=B

=B
DB
DB
DB
DB
DB

=B

=B
DB
PB
PB
PB
PB
PB
PB
PB
PB
PB
JB
JB
JB
VB
\B
\B
\B
bB
bB
bB
hB
hB
uB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
#�B
#�B
#�B
$�B
$�B
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
&�B
'�B
(�B
(�B
(�B
(�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
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
-B
-B
-B
-B
.B
.B
.B
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
49B
49B
49B
49B
5?B
5?B
5?B
6FB
6FB
7LB
7LB
7LB
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
9XB
9XB
:^B
:^B
:^B
:^B
:^B
:^B
:^B
;dB
;dB
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
=qB
=qB
=qB
=qB
=qB
=qB
=qB
=qB
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
@�B
@�B
@�B
A�B
A�B
A�B
A�B
A�B
B�B
A�B
B�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
E�B
E�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
H�B
H�B
I�B
H�B
I�B
I�B
I�B
I�B
I�B
I�B
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
L�B
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
P�B
P�B
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
R�B
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
XB
XB
XB
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
[#B
[#B
[#B
[#B
[#B
[#B
\)B
\)B
\)B
\)B
\)B
]/B
]/B
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
e`B
e`B
ffB
ffB
ffB
ffB
ffB
ffB
ffB
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
k�B
l�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B
gmB
gmB
gRB
gRB
gRB
gRB
gRB
gRB
gmB
gRB
hsB
iyB
jeB
kkB
kkB
lqB
m�B
lqB
m�B
m�B
n�B
t�B
u�B
�B
�BB
��B
ȴB
��B)�BI�Bs�B��B�DB�iB��B�!B�ZB�LB�4B�BB��B�FBB��B�XB�mB�B��B	7B�B�B{�Bx�B_!BjB�B��B��B��B�B�B�B�B�9B�RB�JB�PB�PB�PB�wB�]B�PB�DB�2B�B�-B�B��B��B��B�DB{�Bo�B\BK�BE�B>wB5%B$�B�BgBB�B�PB�TBC{B
��B
��BB
�wB
��B
ʦB
ǔB
�%B
�DB
r�B
>wB
'�B
H�B
=VB
HB
<B
�B
NB	��B	�XB	��B	fLB	I�B	1B	�B��B��B�kB�_B�XB�sB�4B��B�cB�RB�B��B��B��B��B��B��B��B�TB�HB�PB�#B�B�B��B� B|�Bz�B~�B|�By�By�Bw�Bv�Bt�Bs�Br�Br�Bq�Bs�Bn}BkkBhXBc:B_!B^Bc:BcTBd@BeFBe`BcTBfLBeFBdZBb4B`'BZB\BV�BT�BU�BR�BV�BW�B[	B^BaHBa-B`BBbNBfLBi_Bn}Bs�By�Bz�B}�B}�B{�Bw�BhXBa-B]B[	BT�BO�BI�BB�B:^B7LB7LB>]B;JB:DB88B88B88B;dB<PB=VB>]B?cB?}B?cB@iBAoBAoBD�BF�BH�BH�BJ�BJ�BJ�BJ�BJ�BK�BK�BK�BL�BO�BP�BS�BR�BR�BS�BVBVBX�B_!Bb4BcTBeFBgRBi_BkkBp�Bq�Br�Bo�BlqBi_BgRBfLBeFBeFBfLBgRBi_BhXBeFBe`Bd@BfLBkkBmwBmwBq�Bt�Bu�Bu�Bu�Bw�B|�B��B�B�B�1B�#B�0B�6B�BB�HB�gB��B��B��B��B��B��B��B� B�B�B�B�'B�'B�B�-B�B�2B�8B�JB�]B�iB�iBāBŢBƎBƎBȚBȚBȚBʦBɠBˬB��B��B��B�	B�FB�sB�eB�B�}B�B�B�B��B��B��B	�B	B	B	B	0B	<B	HB	uB	gB	sB	sB	sB	yB	�B	�B	!�B	#�B	$�B	%�B	'�B	)�B	*�B	0B	2B	4B	6+B	<PB	>]B	?cB	@iB	C{B	E�B	H�B	I�B	K�B	L�B	N�B	P�B	O�B	N�B	O�B	R�B	V�B	X�B	X�B	ZB	]B	^B	_!B	eFB	gRB	jeB	o�B	n}B	o�B	p�B	s�B	u�B	w�B	{�B	{�B	|�B	��B	��B	��B	�B	�B	�B	�)B	�0B	�HB	�NB	�TB	�yB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�!B	�B	�B	�B	�B	�%B	�%B	�+B	�8B	�RB	�>B	�DB	�PB	�]B	�]B	�cB	��B	�oB	�uB	�uB	āB	ňB	ǔB	ȚB	ȚB	ɺB	ʦB	ʦB	ˬB	��B	οB	οB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�!B	�BB	�BB	�-B	�:B	�:B	�@B	�FB	�LB	�LB	�LB	�RB	�XB	�_B	�yB	�eB	�eB	�eB	�eB	�B	�qB	�B	�wB	�}B	�}B	�B	��B	��B	��B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B	��B
 �B
�B
B
�B
�B
�B
�B
B
B
B
B
B
B
B
B
1B
	B
	7B
	7B
	B

#B

#B

#B
)B
)B
)B
)B
)B

#B

#B
)B
6B
6B
6B
6B
6B
PB
6B
6B
6B
0B
0B
0B
<B
\B
BB
BB
HB
HB
HB
NB
NB
[B
[B
aB
gB
mB
mB
mB
sB
sB
sB
yB
yB
B
B
B
B
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
#�B
#�B
#�B
$�B
$�B
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
&�B
'�B
(�B
(�B
(�B
(�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
*�B
+B
+�B
+�B
,B
+�B
,B
+�B
,�B
,�B
-B
,�B
-�B
-�B
-�B
/ B
/ B
/ B
0B
0B
0B
0B
0B
1B
1B
1B
2B
2B
2B
2B
2B
3B
3B
3B
3B
3B
3B
4B
4B
4B
4B
4B
4B
4B
5%B
5%B
5%B
6+B
6+B
72B
72B
72B
88B
88B
88B
88B
8RB
8RB
88B
88B
9>B
9>B
9>B
9>B
9XB
:DB
:DB
:^B
:DB
:DB
:DB
:DB
;JB
;JB
;JB
;JB
;JB
;JB
<jB
<PB
<PB
<PB
<PB
<PB
=VB
=VB
=VB
=VB
=qB
=VB
=VB
=VB
=VB
>]B
>]B
>]B
?cB
?cB
?cB
?cB
@iB
@iB
@iB
@iB
@iB
@iB
@iB
AoB
AoB
AoB
AoB
A�B
B�B
AoB
B�B
BuB
BuB
BuB
C{B
C{B
C�B
C{B
D�B
D�B
D�B
E�B
E�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
H�B
H�B
I�B
H�B
I�B
I�B
I�B
I�B
I�B
I�B
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
L�B
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
P�B
P�B
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
R�B
S�B
S�B
T�B
T�B
T�B
T�B
T�B
T�B
VB
U�B
VB
U�B
VB
U�B
V�B
V�B
V�B
V�B
V�B
W�B
W�B
W�B
W�B
W�B
XB
X�B
X�B
YB
YB
X�B
ZB
ZB
ZB
ZB
[	B
[	B
[	B
[	B
[	B
[	B
[	B
[	B
[	B
[	B
\)B
\B
\B
\B
\B
]B
]B
^B
^5B
^B
^5B
^B
^5B
_!B
_!B
_!B
_!B
_!B
_!B
_!B
`'B
`'B
`'B
`'B
`'B
`'B
`'B
`'B
`BB
a-B
a-B
a-B
b4B
b4B
b4B
b4B
b4B
c:B
c:B
cTB
d@B
d@B
dZB
d@B
d@B
d@B
d@B
d@B
eFB
e`B
eFB
eFB
e`B
eFB
eFB
fLB
fLB
fLB
ffB
fLB
fLB
fLB
gRB
gRB
hXB
hsB
hsB
hXB
i_B
i_B
iyB
i_B
i_B
i_B
i_B
iyB
jeB
jeB
jeB
jeB
jeB
jeB
jeB
kkB
kkB
kkB
kkB
l�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS error, 0.01(PSS-78))                                                                                                                                                                                                                    SP=-0.18(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201904210038292019042100382920190421003829201904220026382019042200263820190422002638JA  ARFMdecpA19c                                                                20190416183629  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20190416093647  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20190416093648  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20190416093649  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20190416093649  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20190416093649  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20190416093649  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20190416093649  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20190416093650  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20190416093650                      G�O�G�O�G�O�                JA  ARUP                                                                        20190416100237                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20190416153239  CV  JULD            G�O�G�O�FŶs                JM  ARGQJMQC2.0                                                                 20190416153239  CV  JULD_LOCATION   G�O�G�O�FŶ�                JM  ARGQJMQC2.0                                                                 20190416153239  CV  LATITUDE        G�O�G�O�A���                JM  ARGQJMQC2.0                                                                 20190416153239  CV  LONGITUDE       G�O�G�O��-�                JM  ARCAJMQC2.0                                                                 20190420153829  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20190420153829  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20190421152638  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200120031517                      G�O�G�O�G�O�                