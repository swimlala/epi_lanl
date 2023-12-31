CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-02-19T00:35:26Z creation;2018-02-19T00:35:34Z conversion to V3.1;2019-12-19T07:49:02Z update;     
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
_FillValue                 �  I    PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `l   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �X   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �<   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �(   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ܘ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
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
_FillValue                    �(Argo profile    3.1 1.2 19500101000000  20180219003526  20200115121518  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0576_211                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @�Mu���1   @�Mv����@:�@��4n�dcDg8~1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @@  @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CI�fCK�fCN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2�fD3  D3� D4  D4� D5  D5� D6  D6� D7  D7y�D7��D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@y�DA  DA� DB  DB� DC  DC� DD  DD� DEfDE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�P 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @>�R@~�R@�\)@�\)A�A?�A_�A�A��
A��
A��
A��
A��
A��
A��
A��
B�B�B�B�B'�B/�B7�B?�BG�BO�BW�B_�Bg�Bo�Bw�B�B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B�B���B���B���B���B���B���B���B�B���B���B���B���B���B���B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI�HCK�HCM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qD ~�D ��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D	~�D	��D
~�D
��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D ~�D ��D!~�D!��D"~�D"��D#~�D#��D$~�D$��D%~�D%��D&~�D&��D'~�D'��D(~�D(��D)~�D)��D*~�D*��D+~�D+��D,~�D,��D-~�D-��D.~�D.��D/~�D/��D0~�D0��D1~�D1��D2�D2��D3~�D3��D4~�D4��D5~�D5��D6~�D6��D7xRD7�RD8~�D8��D9~�D9��D:~�D:��D;~�D;��D<~�D<��D=~�D=��D>~�D>��D?~�D?��D@xRD@��DA~�DA��DB~�DB��DC~�DC��DD~�DEDE~�DE��DF~�DF��DG~�DG��DH~�DH��DI~�DI��DJ~�DJ��DK~�DK��DL~�DL��DM~�DM��DN~�DN��DO~�DO��DP~�DP��DQ~�DQ��DR~�DR��DS~�DS��DT~�DT��DU~�DU��DV~�DV��DW~�DW��DX~�DX��DY~�DY��DZ~�DZ��D[~�D[��D\~�D\��D]~�D]��D^~�D^��D_~�D_��D`~�D`��Da~�Da��Db~�Db��Dc~�Dc��Dd~�Dd��De~�De��Df~�Df��Dg~�Dg��Dh~�Dh��Di~�Di��Dj~�Dj��Dk~�Dk��Dl~�Dl��Dm~�Dm��Dn~�Dn��Do~�Do��Dp~�Dp��Dq~�Dq��Dr~�Dr��Ds~�Ds��Dt~�Dt��Du~�Du��Dv~�Dv��Dw~�Dw��Dx~�Dx��Dy~�Dy��Dz~�Dz��D{~�D{��D|~�D|��D}~�D}��D~~�D~��D~�D��D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D¿\D��\D�?\D�\Dÿ\D��\D�?\D�\DĿ\D��\D�?\D�\Dſ\D��\D�?\D�\Dƿ\D��\D�?\D�\Dǿ\D��\D�?\D�\Dȿ\D��\D�?\D�\Dɿ\D��\D�?\D�\Dʿ\D��\D�?\D�\D˿\D��\D�?\D�\D̿\D��\D�?\D�\DͿ\D��\D�?\D�\Dο\D��\D�?\D�\DϿ\D��\D�?\D�\Dп\D��\D�?\D�\Dѿ\D��\D�?\D�\Dҿ\D��\D�?\D�\Dӿ\D��\D�?\D�\DԿ\D��\D�?\D�\Dտ\D��\D�?\D�\Dֿ\D��\D�?\D�\D׿\D��\D�?\D�\Dؿ\D��\D�?\D�\Dٿ\D��\D�?\D�\Dڿ\D��\D�?\D�\Dۿ\D��\D�?\D�\Dܿ\D��\D�?\D�\Dݿ\D��\D�?\D�\D޿\D��\D�?\D�\D߿\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D��\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��D�O\111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�JA�VA�JA�JA�
=A�
=A�
=A�JA�JA�JA�VA�
=A�A���A��A��/A��uA�5?A���A��A�p�A�XA�I�A�A�A� �A��9A�t�A�bNA�G�A�9XA�&�A� �A�A��A�A��^A���A��hA��+A�x�A�dZA�\)A�\)A�\)A�\)A�XA�K�A�=qA�/A��A�ƨA�;dA��RA�&�A�|�A�jA�ȴA���A�1A�7LA�/A�1A�ȴA��!A��A��jA��A��FA�oA���A���A��A�t�A�A�A�bNA�A��jA��-A���A��^A�\)A��7A��A�%A��9A��PA�dZA�S�A�G�A�;dA�{A�7A|�Ax�`At��As"�AqdZApE�Ao�An{Al��AkVAj5?Ai�^Ah�/Af�AfI�Ae�;AehsAd��AdbNAdVAd{Ac�^Ac`BAcoAbz�Ab$�Ab$�Ab  Aa�^Aa?}A`�RA_O�A]
=A\bNA[�A[p�AZ-AXAV�DAU�mAT��AS��AS�PAS?}AR�yARJAP�jAOVAN-AM�AL�DAK�PAJVAI�7AHjAG�AF��AFA�AEdZADM�AC�^AC33AB��AB  AA`BAAC�A@�A@{A>�RA=�FA<��A:�HA:  A8��A7�#A7|�A6�A61'A5��A5l�A4n�A3��A2VA1�FA1K�A0��A.�A-��A,��A+�;A+"�A*�jA*r�A*�A)t�A)A(��A(z�A(v�A(E�A'�^A&ffA%�^A$��A#O�A"^5A �+A�AA�RA�#A�7A"�A�\A?}A�TAA��A�A/A�A�AbA��An�AhsA�`A1'A��Ax�A`BAC�AVA�9A{AoAĜA  A�AM�A�-A
��A
  A��AZA�A;dA��AĜAQ�A��A\)A�HAz�A�AQ�A��A �A �@��@�J@��9@��m@�+@�@�C�@�5?@��-@�O�@�%@��/@���@�Z@���@�7L@�I�@��@��@��u@ޏ\@��@�  @ف@�;d@��@� �@�ff@���@�`B@�9X@�K�@�+@��H@Χ�@̛�@ʟ�@ɑh@�?}@��`@�bN@���@�J@�/@�bN@�ȴ@��@�A�@��@���@�?}@�Ĝ@�j@���@�+@�v�@��u@�  @��m@�ƨ@�;d@��@�V@�@��h@��/@��w@�+@�^5@�1'@��F@�t�@�o@��h@�r�@��m@��\@�p�@�I�@��w@�
=@���@�E�@��@���@��h@�V@���@���@�;d@��y@���@�~�@�^5@�5?@���@��h@�V@��m@���@�S�@�ȴ@�~�@�E�@��-@�/@�Q�@�t�@�+@��R@�=q@��@�hs@�%@�j@��@��
@���@�\)@�;d@�33@��@�5?@��-@�7L@��/@��9@���@��@��@���@�E�@��@�hs@��@�33@��\@�M�@�p�@�&�@�V@���@�A�@�ƨ@���@�t�@�\)@�K�@�+@��@�
=@��@��+@�$�@��#@���@��@�`B@�V@��/@��@�Z@�9X@� �@��@��@���@���@�S�@�C�@�C�@�;d@�"�@�33@�o@��\@�{@��@���@�@���@��#@��T@���@��@��#@��@�?}@��@���@��@��D@�b@���@�@���@�v�@�=q@���@�p�@�G�@�?}@�?}@��9@�@�P@+@~v�@~{@}�@|��@{�
@z��@z-@y�#@yG�@y%@xA�@w�P@v��@vȴ@v��@vV@v$�@v5?@u�h@tz�@s�m@s�@s"�@r�@rn�@q��@q�@q�#@q��@p�@pA�@p1'@pA�@p1'@pb@o��@o�@o�P@o;d@n��@n�R@nff@n5?@m�@m@mO�@m�@mV@mV@m/@l�@lj@l�D@lZ@k��@ko@j�!@j^5@j-@jJ@i�#@i�^@i��@iX@h �@g�w@g�@g�@g;d@g�w@gl�@f�R@fV@f�+@f��@f�+@e�@e�-@e`B@e?}@d�@d�D@d9X@c�
@cdZ@cC�@c@b�\@b=q@b-@a�#@aG�@`�9@`Q�@_�P@_+@^5?@]@]�h@]O�@\�/@\Z@[t�@[S�@["�@ZM�@Y��@Yhs@XĜ@X�@X �@W�@W�@WK�@V�y@V��@Vff@U��@U�@Up�@U�@T�/@T�@T�@St�@R��@R�\@RM�@Q��@Q�@P��@P��@P�@PA�@O�;@OK�@N�@N�y@N�R@N��@N��@N��@N��@Nff@M�h@M`B@M?}@M�@MV@L��@L�@L�@LZ@L�@K�
@Kƨ@K��@K@J��@J�\@JM�@JM�@J�@I�@I�#@I��@IG�@I&�@H��@Hb@G�w@G��@G|�@GK�@Fȴ@F�+@F{@E�h@D��@D�@Dz�@DI�@D(�@C��@C�F@Ct�@C33@B��@B�!@B^5@A��@Ax�@AX@AG�@A7L@@��@@Ĝ@@�@@A�@@  @?�;@?�@?|�@?l�@?
=@>�R@>v�@>5?@=�T@=��@=�h@=O�@<��@<��@<z�@<(�@<�@<1@;�F@;��@;��@;��@;��@;�@;t�@;C�@;"�@:�@:��@:�!@:M�@:�@9�^@9X@9�@9%@8��@8Ĝ@8Ĝ@8�9@8��@8��@8��@8��@8�u@8A�@81'@8b@7�@7��@7K�@7
=@6ȴ@6��@6v�@6E�@6@6@5�@5@5�@4��@4�j@4j@4I�@49X@49X@4(�@4�@41@3�m@3��@3@2^5@2=q@1�@1X@1&�@1&�@1&�@1&�@1&�@1�@1�@1%@0��@0Ĝ@0��@0�u@0A�@/�P@/
=@.��@.ff@.V@.E�@.5?@.{@-�@-@-��@-p�@-O�@-�@-�@,�@,��@,I�@+�m@+��@+�@+dZ@+S�@+33@+o@*�@*�@*��@*n�@)��@)�7@)&�@(�`@(��@(�u@(A�@'�;@'�@'\)@&��@&v�@&$�@%�T@%��@%?}@$�/@$�D@$I�@#ƨ@#�@#33@#@"=q@!��@!%@ �9@ �u@ �@ r�@ bN@ A�@ b@�P@
=@�y@ȴ@��@�+@ff@5?@p�@/@V@�@�@�/@��@j@(�@1@��@�m@��@C�@��@~�@M�@J@��@��@�^@x�@7L@�`@�9@�@ �@��@��@;d@
=@�@��@��@�+@�+@ff@E�@$�@{@�@�T@�-@�h@p�@p�@p�@p�@O�@��@��@9X@(�@1@1@1@1@�
@��@S�@@�H@��@�!@�!@�\@~�@n�@M�@-@-@�@J@�@�^@��@�@�9@A�@  @�@�P@K�@;d@+@�@�y@�@ȴ@�R@��@V@{@{@@�@�T@@�-@p�@O�@��@�/@�j@�@��@�D@z�@z�@z�@j@9X@�m@�F@S�@@
��@
��@
^5@
J@	�#@	��@	��@	x�@	hs@	X@	G�@	&�@	�@	�@	%@��@�@A�@1'@b@�@|�@;d@�@��@��@�+@v�@v�@v�@5?@@�T@��@�-@�h@p�@O�@�@V@��@�@�/@��@�@�@j@(�@(�@�@1@ƨ@��@�@C�@33@"�@o@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�JA�VA�JA�JA�
=A�
=A�
=A�JA�JA�JA�VA�
=A�A���A��A��/A��uA�5?A���A��A�p�A�XA�I�A�A�A� �A��9A�t�A�bNA�G�A�9XA�&�A� �A�A��A�A��^A���A��hA��+A�x�A�dZA�\)A�\)A�\)A�\)A�XA�K�A�=qA�/A��A�ƨA�;dA��RA�&�A�|�A�jA�ȴA���A�1A�7LA�/A�1A�ȴA��!A��A��jA��A��FA�oA���A���A��A�t�A�A�A�bNA�A��jA��-A���A��^A�\)A��7A��A�%A��9A��PA�dZA�S�A�G�A�;dA�{A�7A|�Ax�`At��As"�AqdZApE�Ao�An{Al��AkVAj5?Ai�^Ah�/Af�AfI�Ae�;AehsAd��AdbNAdVAd{Ac�^Ac`BAcoAbz�Ab$�Ab$�Ab  Aa�^Aa?}A`�RA_O�A]
=A\bNA[�A[p�AZ-AXAV�DAU�mAT��AS��AS�PAS?}AR�yARJAP�jAOVAN-AM�AL�DAK�PAJVAI�7AHjAG�AF��AFA�AEdZADM�AC�^AC33AB��AB  AA`BAAC�A@�A@{A>�RA=�FA<��A:�HA:  A8��A7�#A7|�A6�A61'A5��A5l�A4n�A3��A2VA1�FA1K�A0��A.�A-��A,��A+�;A+"�A*�jA*r�A*�A)t�A)A(��A(z�A(v�A(E�A'�^A&ffA%�^A$��A#O�A"^5A �+A�AA�RA�#A�7A"�A�\A?}A�TAA��A�A/A�A�AbA��An�AhsA�`A1'A��Ax�A`BAC�AVA�9A{AoAĜA  A�AM�A�-A
��A
  A��AZA�A;dA��AĜAQ�A��A\)A�HAz�A�AQ�A��A �A �@��@�J@��9@��m@�+@�@�C�@�5?@��-@�O�@�%@��/@���@�Z@���@�7L@�I�@��@��@��u@ޏ\@��@�  @ف@�;d@��@� �@�ff@���@�`B@�9X@�K�@�+@��H@Χ�@̛�@ʟ�@ɑh@�?}@��`@�bN@���@�J@�/@�bN@�ȴ@��@�A�@��@���@�?}@�Ĝ@�j@���@�+@�v�@��u@�  @��m@�ƨ@�;d@��@�V@�@��h@��/@��w@�+@�^5@�1'@��F@�t�@�o@��h@�r�@��m@��\@�p�@�I�@��w@�
=@���@�E�@��@���@��h@�V@���@���@�;d@��y@���@�~�@�^5@�5?@���@��h@�V@��m@���@�S�@�ȴ@�~�@�E�@��-@�/@�Q�@�t�@�+@��R@�=q@��@�hs@�%@�j@��@��
@���@�\)@�;d@�33@��@�5?@��-@�7L@��/@��9@���@��@��@���@�E�@��@�hs@��@�33@��\@�M�@�p�@�&�@�V@���@�A�@�ƨ@���@�t�@�\)@�K�@�+@��@�
=@��@��+@�$�@��#@���@��@�`B@�V@��/@��@�Z@�9X@� �@��@��@���@���@�S�@�C�@�C�@�;d@�"�@�33@�o@��\@�{@��@���@�@���@��#@��T@���@��@��#@��@�?}@��@���@��@��D@�b@���@�@���@�v�@�=q@���@�p�@�G�@�?}@�?}@��9@�@�P@+@~v�@~{@}�@|��@{�
@z��@z-@y�#@yG�@y%@xA�@w�P@v��@vȴ@v��@vV@v$�@v5?@u�h@tz�@s�m@s�@s"�@r�@rn�@q��@q�@q�#@q��@p�@pA�@p1'@pA�@p1'@pb@o��@o�@o�P@o;d@n��@n�R@nff@n5?@m�@m@mO�@m�@mV@mV@m/@l�@lj@l�D@lZ@k��@ko@j�!@j^5@j-@jJ@i�#@i�^@i��@iX@h �@g�w@g�@g�@g;d@g�w@gl�@f�R@fV@f�+@f��@f�+@e�@e�-@e`B@e?}@d�@d�D@d9X@c�
@cdZ@cC�@c@b�\@b=q@b-@a�#@aG�@`�9@`Q�@_�P@_+@^5?@]@]�h@]O�@\�/@\Z@[t�@[S�@["�@ZM�@Y��@Yhs@XĜ@X�@X �@W�@W�@WK�@V�y@V��@Vff@U��@U�@Up�@U�@T�/@T�@T�@St�@R��@R�\@RM�@Q��@Q�@P��@P��@P�@PA�@O�;@OK�@N�@N�y@N�R@N��@N��@N��@N��@Nff@M�h@M`B@M?}@M�@MV@L��@L�@L�@LZ@L�@K�
@Kƨ@K��@K@J��@J�\@JM�@JM�@J�@I�@I�#@I��@IG�@I&�@H��@Hb@G�w@G��@G|�@GK�@Fȴ@F�+@F{@E�h@D��@D�@Dz�@DI�@D(�@C��@C�F@Ct�@C33@B��@B�!@B^5@A��@Ax�@AX@AG�@A7L@@��@@Ĝ@@�@@A�@@  @?�;@?�@?|�@?l�@?
=@>�R@>v�@>5?@=�T@=��@=�h@=O�@<��@<��@<z�@<(�@<�@<1@;�F@;��@;��@;��@;��@;�@;t�@;C�@;"�@:�@:��@:�!@:M�@:�@9�^@9X@9�@9%@8��@8Ĝ@8Ĝ@8�9@8��@8��@8��@8��@8�u@8A�@81'@8b@7�@7��@7K�@7
=@6ȴ@6��@6v�@6E�@6@6@5�@5@5�@4��@4�j@4j@4I�@49X@49X@4(�@4�@41@3�m@3��@3@2^5@2=q@1�@1X@1&�@1&�@1&�@1&�@1&�@1�@1�@1%@0��@0Ĝ@0��@0�u@0A�@/�P@/
=@.��@.ff@.V@.E�@.5?@.{@-�@-@-��@-p�@-O�@-�@-�@,�@,��@,I�@+�m@+��@+�@+dZ@+S�@+33@+o@*�@*�@*��@*n�@)��@)�7@)&�@(�`@(��@(�u@(A�@'�;@'�@'\)@&��@&v�@&$�@%�T@%��@%?}@$�/@$�D@$I�@#ƨ@#�@#33@#@"=q@!��@!%@ �9@ �u@ �@ r�@ bN@ A�@ b@�P@
=@�y@ȴ@��@�+@ff@5?@p�@/@V@�@�@�/@��@j@(�@1@��@�m@��@C�@��@~�@M�@J@��@��@�^@x�@7L@�`@�9@�@ �@��@��@;d@
=@�@��@��@�+@�+@ff@E�@$�@{@�@�T@�-@�h@p�@p�@p�@p�@O�@��@��@9X@(�@1@1@1@1@�
@��@S�@@�H@��@�!@�!@�\@~�@n�@M�@-@-@�@J@�@�^@��@�@�9@A�@  @�@�P@K�@;d@+@�@�y@�@ȴ@�R@��@V@{@{@@�@�T@@�-@p�@O�@��@�/@�j@�@��@�D@z�@z�@z�@j@9X@�m@�F@S�@@
��@
��@
^5@
J@	�#@	��@	��@	x�@	hs@	X@	G�@	&�@	�@	�@	%@��@�@A�@1'@b@�@|�@;d@�@��@��@�+@v�@v�@v�@5?@@�T@��@�-@�h@p�@O�@�@V@��@�@�/@��@�@�@j@(�@(�@�@1@ƨ@��@�@C�@33@"�@o@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   BBBBBB%B%B%BBBBBB��B��B�B�B�fB�HB��B��BB��B��B�B�B��B��BBB%BBBB%BBBBBB%B+B+B%BBB��B��B��B�yB�B��B�^B��B~�BK�BBB
=B��B�BƨB�!B�hBt�B8RB
��B)�B%�BbB
�B
�NB
��B
��B
��B
��B
��B
�oB
��B
��B
�%B
�7B
�hB
�DB
�1B
�1B
�%B
�B
z�B
k�B
S�B
$�B	��B	�mB	��B	��B	��B	�B	�fB	�5B	��B	��B	��B	ƨB	�XB	ÖB	ȴB	ŢB	��B	ŢB	ȴB	ŢB	B	��B	��B	�qB	�}B	ÖB	�wB	�RB	�B	��B	�uB	�B	�oB	�{B	�7B	x�B	cTB	e`B	n�B	e`B	bNB	l�B	gmB	aHB	R�B	I�B	>wB	B�B	A�B	D�B	9XB	/B	0!B	'�B	�B	$�B	&�B	�B	hB	�B	�B	�B	DB	
=B	\B	1B��B�B�yB�mB��B�#B��B�B�#B��B�B��B��BŢBÖB�3B�wB�jB�9B��B��B��B��B��B�B�B��B��B��B��B��B��B��B�{B�B�7B�Bp�By�BffBu�Bz�B}�Bs�Bw�Bs�BhsB^5BZB^5BiyBbNB_;BffBdZB\)BO�BW
BO�BVBVBW
B^5B_;B\)BXBR�BK�BB�BK�BC�BA�B7LB:^B5?B7LB/B:^B5?B@�B?}B?}B9XB5?B9XB49B1'B!�B%�B(�B%�B'�B)�B%�B$�B'�B#�B�BoB �B&�B'�B%�B!�B�B��B�)B�BuB%BB��BBuBBB+BB�B\B�B�B�B�B%�B!�B�BoBhB�B'�B&�B#�B�B �B �B!�B�B�B"�B#�B$�B(�B-B,B+B(�B(�B"�B2-B9XB7LB49B5?B49B49B7LB1'B/B2-B33B+B@�BC�B@�B9XB<jBE�B?}BE�BF�BQ�BR�BXBXB[#B\)B[#BXBT�B`BBaHBdZBffBhsBiyBhsBe`Be`BbNB^5BjBm�Bk�Bo�Bq�Bn�Bo�Bq�Bs�B� B~�B� B�B�B�%B�1B�VB�bB�uB�{B��B��B�uB�uB��B��B��B��B��B��B��B�B�!B�B�B�B�B�jBĜBB��B��B��B��B��B�#B�/B�;B�NB�TB�fB�mB�mB�B�B��B��B��B��B��B	  B	B	B	+B	1B		7B	1B	+B	%B	DB	\B	hB	uB	{B	�B	�B	�B	�B	�B	 �B	"�B	%�B	'�B	'�B	(�B	'�B	(�B	'�B	+B	/B	.B	2-B	2-B	/B	33B	6FB	?}B	?}B	A�B	A�B	?}B	C�B	E�B	D�B	@�B	@�B	K�B	L�B	L�B	N�B	O�B	O�B	P�B	S�B	[#B	_;B	aHB	dZB	dZB	ffB	jB	n�B	o�B	o�B	p�B	q�B	n�B	n�B	s�B	v�B	x�B	{�B	{�B	~�B	�B	�B	�B	~�B	�+B	�=B	�JB	�PB	�VB	�\B	�uB	�uB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�3B	�RB	�^B	�RB	�LB	�^B	�wB	��B	��B	��B	ĜB	ǮB	��B	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�)B	�5B	�5B	�5B	�5B	�;B	�TB	�TB	�HB	�`B	�`B	�fB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
  B
B
%B
%B
%B
%B
%B
B
B
%B
+B
1B
+B
%B
	7B
	7B

=B
JB
DB
DB
DB

=B

=B
DB

=B
1B
JB
VB
VB
PB
JB
PB
PB
JB
VB
\B
hB
hB
hB
bB
bB
\B
bB
bB
hB
hB
bB
hB
{B
�B
�B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
#�B
$�B
$�B
$�B
$�B
#�B
%�B
%�B
%�B
$�B
%�B
&�B
&�B
'�B
'�B
'�B
(�B
)�B
)�B
(�B
(�B
'�B
)�B
,B
-B
-B
.B
-B
-B
,B
,B
+B
)�B
+B
.B
.B
.B
1'B
2-B
2-B
2-B
2-B
2-B
2-B
2-B
1'B
2-B
2-B
1'B
0!B
/B
1'B
33B
5?B
6FB
6FB
6FB
6FB
6FB
6FB
7LB
6FB
7LB
7LB
8RB
7LB
7LB
6FB
7LB
8RB
9XB
:^B
:^B
:^B
:^B
;dB
;dB
:^B
9XB
9XB
:^B
;dB
<jB
=qB
<jB
<jB
<jB
=qB
=qB
=qB
=qB
>wB
?}B
?}B
?}B
@�B
@�B
A�B
A�B
B�B
B�B
B�B
@�B
A�B
C�B
D�B
F�B
F�B
G�B
F�B
F�B
E�B
D�B
E�B
H�B
H�B
H�B
H�B
H�B
G�B
F�B
I�B
K�B
K�B
K�B
K�B
J�B
K�B
K�B
L�B
L�B
L�B
K�B
K�B
K�B
M�B
N�B
N�B
O�B
O�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
P�B
P�B
Q�B
R�B
R�B
S�B
T�B
T�B
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
VB
T�B
S�B
S�B
T�B
W
B
W
B
XB
XB
XB
W
B
VB
W
B
W
B
YB
YB
YB
ZB
ZB
ZB
ZB
YB
ZB
ZB
ZB
ZB
YB
YB
YB
W
B
XB
YB
[#B
[#B
\)B
\)B
]/B
]/B
^5B
]/B
^5B
^5B
^5B
^5B
^5B
^5B
`BB
`BB
`BB
`BB
`BB
`BB
_;B
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
aHB
aHB
`BB
aHB
`BB
aHB
bNB
bNB
bNB
cTB
dZB
e`B
dZB
e`B
e`B
ffB
e`B
ffB
ffB
ffB
e`B
e`B
e`B
ffB
gmB
gmB
ffB
gmB
gmB
gmB
iyB
jB
jB
jB
jB
jB
iyB
iyB
jB
k�B
jB
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
l�B
k�B
l�B
m�B
m�B
m�B
l�B
l�B
m�B
m�B
n�B
n�B
n�B
n�B
o�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   BBBBBB%B%B%BBBB'B'B�.B�JB�B�B��B��B�+B�B B�.B�JB��B�AB��B�(B3BSB?BgBaB[B?BSBMB9BMBMB?B+B+B%B3B'B�.B�B�%B�eB�eB�B��B��B�ABPB	BfBJB�RB�BɠB�B�MBx�B>�B�B+QB'8B�B
��B
�B
�B
�*B
�uB
��B
��B
�B
�VB
��B
��B
�#B
��B
��B
��B
��B
�tB
�-B
{B
l=B
U�B
(�B
�B	��B	�B	��B	�0B	�!B	��B	��B	��B	�B	յB	�B	�dB	�MB	�7B	�?B	�UB	��B	��B	�B	�B	�B	�B	�(B	��B	ðB	��B	��B	��B	��B	�gB	��B	�&B	�B	�=B	z�B	fB	gB	o�B	f�B	cTB	l�B	g�B	a�B	TaB	KxB	@�B	C�B	B�B	EmB	:�B	0�B	1[B	)yB	!bB	%�B	'�B	�B	�B	7B	9B	$B	~B	B	�B	�B�$B�qB�B��B�&B�]B��B�	B��B�B�B՛BЗB��BāB�ZB�.B�<B�tB�B�2B�tB��B��B��B��B��B��B��B�>B�`B�B�BB��B��B�XB��Br�B{0Bh�Bv�B{�B~wBt�BxlBtnBi�B`B[�B_VBj0BcTB`\Bf�Bd�B]/BQ�BW�BQ4BV�BV�BW�B^�B_pB\xBX�BS�BL�BC�BLJBD�BBuB8�B;dB6�B8RB0oB;0B6zB@�B@ B@ B:*B6FB9�B5B2B#�B'B*B'8B(�B*�B&�B%�B(�B$�B �BB!�B'RB(>B&2B"NBsB��B��BBaB�B�B�	BYB�B�B�B�BtB+B�BBCBkBdB%�B"4BOB�B�B�B(>B'RB$tB�B!|B!|B"�B�B�B#�B$�B%�B)�B-wB,qB+�B)�B)�B#�B2�B9�B7�B4�B5�B4�B4�B7�B1�B/�B2�B3�B,qB@�BC�BA B:xB=<BF%B@�BFtBGzBRTBS�BX_BXyB[qB\xB[qBX�BU�B`�Ba�Bd�Bf�Bh�Bi�Bh�Be�Be�Bb�B_!Bj�Bm�Bk�Bo�Bq�Bo5Bp;Br|BtnB�4BcB�iB�aB��B��B��B��B��B��B��B��B��B��B��B��B�B�B� B�\B�@B�_B�cB�oB��B��B��B�/B��B�B�-B��B��B�"B�BB�aB�=B�dB�pB�hB�B�B�B�B��B��B��B�	B�B�B�6B	 4B	AB	aB	_B	fB		RB	KB	zB	tB	xB	vB	�B	�B	�B	�B	�B	�B	�B	�B	 �B	"�B	%�B	'�B	'�B	(�B	($B	)B	(>B	+QB	/5B	.}B	2GB	2aB	/�B	3�B	6�B	?�B	?�B	A�B	A�B	?�B	C�B	E�B	D�B	AB	AB	K�B	MB	MB	OB	P.B	P.B	QhB	TaB	[qB	_pB	a|B	d�B	d�B	f�B	j�B	n�B	o�B	o�B	p�B	q�B	n�B	oB	s�B	v�B	y	B	|B	|B	.B	�B	�'B	�;B	cB	�EB	�=B	�JB	�jB	�pB	�vB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�B	�B	�B	�$B	�*B	�B	�*B	�B	�B	�DB	�fB	�CB	�IB	�3B	�8B	�DB	��B	��B	�xB	�wB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	� B	� B	�B	�B	�B	�@B	�:B	�,B	�MB	�MB	�EB	�_B	�]B	�OB	�OB	�jB	�jB	ߊB	�nB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�B	�B	��B	��B	�B	�B	�B	�B	�"B
B
'B
B
B
B
'B
 B
 iB
3B
?B
?B
%B
?B
?B
9B
9B
YB
EB
KB
EB
YB
	lB
	lB

XB
JB
^B
^B
xB

XB

XB
^B

rB
�B
dB
pB
pB
�B
~B
jB
�B
~B
�B
�B
�B
�B
�B
�B
}B
vB
}B
}B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
#�B
$�B
$�B
$�B
$�B
$B
%�B
%�B
%�B
%B
%�B
'B
'B
(
B
(
B
(
B
)B
)�B
*B
)B
)B
($B
*0B
,=B
-)B
-)B
.B
-)B
-)B
,"B
,=B
+6B
*0B
+6B
./B
.IB
.IB
1'B
2-B
2-B
2-B
2-B
2-B
2GB
2-B
1AB
2GB
2GB
1[B
0UB
/iB
1[B
3�B
5ZB
6`B
6FB
6`B
6`B
6`B
6`B
7fB
6zB
7fB
7fB
8RB
7fB
7fB
6�B
7fB
8lB
9rB
:xB
:^B
:xB
:xB
;dB
;dB
:xB
9�B
9�B
:�B
;B
<�B
=qB
<�B
<�B
<�B
=�B
=�B
=�B
=�B
>�B
?�B
?�B
?�B
@�B
@�B
A�B
A�B
B�B
B�B
B�B
@�B
A�B
C�B
D�B
F�B
F�B
G�B
F�B
F�B
E�B
D�B
E�B
H�B
H�B
H�B
H�B
H�B
G�B
F�B
I�B
K�B
K�B
K�B
K�B
J�B
K�B
K�B
L�B
L�B
L�B
K�B
K�B
LB
M�B
N�B
N�B
O�B
O�B
N�B
N�B
OB
N�B
O�B
O�B
O�B
O�B
QB
QB
R B
SB
SB
S�B
T�B
T�B
TB
TB
TB
UB
UB
UB
UB
UB
VB
VB
VB
VB
UB
TB
TB
U2B
W
B
W$B
XB
XB
XB
W$B
VB
W$B
W$B
Y1B
Y1B
YB
ZB
Z7B
ZB
ZB
Y1B
Z7B
ZB
ZB
ZB
Y1B
Y1B
Y1B
W?B
XEB
YKB
[=B
[=B
\CB
\CB
]IB
]IB
^OB
]IB
^5B
^OB
^OB
^OB
^OB
^OB
`BB
`BB
`\B
`BB
`\B
`BB
_VB
`\B
`\B
abB
abB
abB
bNB
bhB
bNB
bNB
bNB
abB
abB
`vB
abB
`vB
abB
bhB
bhB
bhB
cnB
dtB
e`B
dtB
e�B
e`B
ffB
ezB
f�B
ffB
f�B
e�B
ezB
ezB
f�B
gmB
g�B
f�B
g�B
g�B
g�B
i�B
j�B
jB
jB
j�B
jB
i�B
i�B
j�B
k�B
j�B
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
l�B
k�B
l�B
m�B
m�B
m�B
l�B
l�B
m�B
m�B
n�B
n�B
n�B
n�B
o�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.02(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201802230035592018022300355920180223003559201806221237562018062212375620180622123756201804050434472018040504344720180405043447  JA  ARFMdecpA19c                                                                20180219093520  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180219003526  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180219003529  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180219003530  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180219003531  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180219003531  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180219003531  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180219003531  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180219003534  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180219003534                      G�O�G�O�G�O�                JA  ARUP                                                                        20180219005618                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180219153509  CV  JULD            G�O�G�O�F�k�                JM  ARCAJMQC2.0                                                                 20180222153559  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180222153559  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404193447  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622033756  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115121518                      G�O�G�O�G�O�                