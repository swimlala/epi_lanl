CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-04-08T00:35:22Z creation;2018-04-08T00:35:28Z conversion to V3.1;2019-12-19T07:45:21Z update;     
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
_FillValue                    �<Argo profile    3.1 1.2 19500101000000  20180408003522  20200115121516  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0576_227                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @�Ytio�1   @�Yu'�} @9�������dl��Z�1   GPS     A   A   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�ff@���A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{fD{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ Dͼ�D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�0 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�@�(�@�\)A�A?�A_�A�A��
A��
A��
A��
A��
A��
A��
A��
B�B�B�B�B'�B/�B7�B?�BG�BO�BW�B_�Bg�Bo�Bw�B�B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qD ~�D ��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D	~�D	��D
~�D
��D~�D��D~�D��D~�D��D~�D�RD~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D ~�D ��D!~�D!��D"~�D"��D#~�D#��D$~�D$��D%~�D%��D&~�D&��D'~�D'��D(~�D(��D)~�D)��D*~�D*��D+~�D+��D,~�D,��D-~�D-��D.~�D.��D/~�D/��D0~�D0��D1~�D1��D2~�D2��D3~�D3��D4~�D4��D5~�D5��D6~�D6��D7~�D7��D8~�D8��D9~�D9��D:~�D:��D;~�D;��D<~�D<��D=~�D=��D>~�D>��D?~�D?��D@~�D@��DA~�DA��DB~�DB��DC~�DC��DD~�DD��DE~�DE��DF~�DF��DG~�DG��DH~�DH��DI~�DI��DJ~�DJ��DK~�DK��DL~�DL��DM~�DM��DN~�DN��DO~�DO��DP~�DP��DQ~�DQ��DR~�DR��DS~�DS��DT~�DT��DU~�DU��DV~�DV��DW~�DW��DX~�DX��DY~�DY��DZ~�DZ��D[~�D[��D\~�D\��D]~�D]��D^~�D^��D_~�D_��D`~�D`��Da~�Da��Db~�Db��Dc~�Dc��Dd~�Dd��De~�De��Df~�Df��Dg~�Dg��Dh~�Dh��Di~�Di��Dj~�Dj��Dk~�Dk��Dl~�Dl��Dm~�Dm��Dn~�Dn��Do~�Do��Dp~�Dp��Dq~�Dq��Dr~�Dr��Ds~�Ds��Dt~�Dt��Du~�Du��Dv~�Dv��Dw~�Dw��Dx~�Dx��Dy~�Dy��Dz~�D{D{~�D{��D|~�D|��D}~�D}��D~~�D~��D~�D��D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�B�D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D¿\D��\D�?\D�\Dÿ\D��\D�?\D�\DĿ\D��\D�?\D�\Dſ\D��\D�?\D�\Dƿ\D��\D�?\D�\Dǿ\D��\D�?\D�\Dȿ\D��\D�?\D�\Dɿ\D��\D�?\D�\Dʿ\D��\D�?\D�\D˿\D��\D�?\D�\D̿\D��\D�?\D�\Dͼ)D��\D�?\D�\Dο\D��\D�?\D�\DϿ\D��\D�?\D�\Dп\D��\D�?\D�\Dѿ\D��\D�?\D�\Dҿ\D��\D�?\D�\Dӿ\D��\D�?\D�\DԿ\D��\D�?\D�\Dտ\D��\D�?\D�\Dֿ\D��\D�?\D�\D׿\D��\D�?\D�\Dؿ\D��\D�?\D�\Dٿ\D��\D�?\D�\Dڿ\D��\D�?\D�\Dۿ\D��\D�?\D�\Dܿ\D��\D�?\D�\Dݿ\D��\D�?\D�\D޿\D��\D�?\D�\D߿\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D��\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��D�/\1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��#A���A��A��^A�I�A��A��DA���A�A���A���A�|�A�33A��uA�ȴA�x�A�bNA�XA�=qA�VA�ƨA�E�A�(�A�bA���A��DA�A�A�bA��jA�v�A�9XA��A��A�r�A�n�A�O�A�(�A���A�A�VA�VA���A��mA��A���A���A�ffA�v�A�p�A�$�A��FA�&�A��A�1'A�ffA�n�A���A���A�|�A��^A�  A��A��RA��PA� �A��RA���A���A�A��TA�hsA��A�;A{;dAy�Aw�PAv��Aut�Au%Asl�AqVAm?}AkoAiAg�wAfA�Ac��AbAa�wA`�!A_�7A]��A[�FAZ�`AY�AYK�AX��AX�+AW��AU��AS�-ARv�AQ�^AQO�AO�ANr�AN �AM�#AM\)AL5?AKS�AJ�!AJ1'AH�yAHJAG�7AGl�AG?}AF��AFn�AE��ADz�AC�
AChsAA�AAoA@�+A@M�A@�A?�mA?ƨA?��A?XA>�!A=?}A<��A;�A;�7A;7LA:�yA:v�A9�#A9/A8��A85?A7��A6��A5��A3�A3�PA3dZA37LA2��A2 �A1�A0v�A.bNA-K�A,�yA,jA+��A+dZA+�A*�A)�FA);dA(��A(v�A(VA'A';dA&�jA&E�A%��A%��A%�hA%dZA$�A$v�A#A"{A ��A|�A��AjA��A�AO�A�A(�A�RA��A��A�7AdZA%A�\A��A�-AĜA^5A�#A?}AZA �A`BA��AƨA33A�9AM�A��A\)A��AQ�AG�A
=qA	VAĜA��A�A�A�PAĜA  A�-A�HA�;AK�A�9AE�A�A�-A ��A bNA $�A  �A �@��;@�J@���@���@��+@�{@�X@��@�b@�P@�+@���@�@�V@��@��@�bN@�$�@�?}@�I�@�ȴ@��@�A�@��@�bN@�/@ݩ�@�"�@�|�@֗�@�M�@�{@�@���@պ^@Ձ@�O�@�Z@љ�@��y@�@�%@�1@��H@ʗ�@�@��/@�@�V@�(�@°!@��@��@���@��;@���@�x�@�  @���@�$�@���@�p�@���@��@��y@���@�ff@�-@�{@��T@��@��R@�/@���@��w@���@�t�@��@�@��h@�&�@��u@���@��@��R@�n�@���@���@�Z@��@�K�@�E�@�@��#@�@���@��D@��F@�
=@��y@���@��!@�ȴ@�ȴ@��!@��!@��!@���@�ff@�V@�=q@�5?@��@�G�@���@�b@���@�\)@�@�v�@��T@���@��D@�I�@�(�@�ƨ@�\)@�+@���@�$�@�`B@���@�j@��
@���@�33@��@���@���@�/@�V@���@���@�bN@� �@�ƨ@�C�@��y@�ȴ@���@�n�@���@�/@�Ĝ@�z�@�(�@��
@�@�x�@��/@��u@� �@��@���@��P@�
=@���@���@�^5@�M�@�5?@��T@�@��7@�O�@��`@�  @��@��P@�\)@���@�ȴ@��!@��!@���@�n�@�E�@�J@�@�hs@�?}@�%@��`@���@�Ĝ@���@�A�@l�@~�@}��@|�@|z�@|I�@{��@{�@{o@z=q@x��@w\)@v�y@v@u�-@u/@t��@t��@t��@t��@t��@s��@s"�@r=q@q��@q&�@p�@o�@o+@n�+@n{@n@m��@m�@l��@l9X@kt�@ko@j�!@j�\@j~�@j^5@i�@ix�@iX@i&�@h��@hĜ@h��@h�u@h�u@h�@hbN@hb@g�;@g�w@g�@g��@g��@g|�@g\)@f��@fȴ@fV@e�-@e�@e`B@eO�@e?}@d�@d9X@d�@c�
@c��@c33@b�\@bn�@b=q@b�@a��@a�@`��@`r�@`A�@`  @_��@_�@_��@_�P@_l�@_;d@_+@^��@^E�@^5?@^{@]�T@]@]�-@]p�@\��@\�@\Z@\�@[�m@[�@Z�H@ZM�@Y��@Y�#@Y�7@Yhs@YX@Y7L@Y�@X��@X��@X�u@W�@W��@Wl�@Vȴ@V��@V�R@V�+@V$�@U��@U�-@U�h@U`B@U/@T�@T9X@T1@S��@S33@R��@RM�@Q��@Q�^@Q�7@QX@Q7L@Q%@P�u@P1'@O��@O��@O;d@N��@Nȴ@N��@N�+@M�T@M?}@L�j@L�D@Lz�@L9X@L�@K�m@K��@KC�@J�H@J��@J�!@J��@J�\@J�\@J�\@Jn�@JM�@I��@I�7@I&�@H�`@H��@HA�@G�@G
=@F�+@F@E�-@E�h@E/@D�@Dj@D9X@C�@B�@B��@B��@B��@B�\@B-@A�#@A��@A��@A��@AG�@A�@@�@@  @?�w@?�P@?\)@?�@>��@>�R@>�+@>�+@>v�@>ff@>ff@>5?@>{@=�-@=�h@=O�@<�j@<j@<(�@;ƨ@;��@;S�@;@:�H@:�!@:~�@:�@9�7@9&�@8�`@8��@8A�@7�w@7;d@6�y@6��@5�T@5?}@4��@4�D@4�@3�F@3�@3"�@2~�@2n�@2n�@2n�@2n�@2^5@2M�@2=q@1��@1��@0�`@0��@0�`@0��@0��@0Ĝ@0r�@/��@/K�@/�@.�@.v�@.5?@.$�@-�@-��@-@-�-@-�h@-p�@-?}@-/@,��@,�@,�j@,z�@+�@+S�@+33@*�!@*�\@*�\@*^5@)�^@)7L@(�9@(bN@(1'@(1'@(1'@'��@'�@'�@'
=@'
=@'
=@'�@'�@&�@&5?@%��@%�h@%�h@%�@%`B@%/@$��@$�j@$�D@$I�@$�@#ƨ@#�@#C�@#"�@#o@#o@#@"�H@"��@"��@"^5@!hs@!7L@!�@ ��@ �9@  �@�@�P@�@��@�@�R@v�@{@�@�T@��@@�-@�@�@I�@�
@��@�@S�@33@"�@o@@@�@~�@^5@��@&�@�9@�u@r�@bN@Q�@A�@ �@b@�;@�w@��@l�@ȴ@�+@E�@$�@{@@�-@�h@p�@/@�@��@9X@1@1@��@33@"�@"�@��@~�@=q@��@�@�#@��@��@�7@x�@X@G�@&�@%@��@�`@��@�9@�9@��@Q�@ �@b@l�@+@��@ȴ@��@��@�+@v�@v�@V@V@E�@5?@@@@@�T@��@�-@�@O�@�@��@��@9X@ƨ@��@dZ@S�@"�@o@
�H@
�\@
-@	�#@	�^@	��@	�7@	G�@	&�@	%@��@�`@��@�9@�u@r�@A�@1'@ �@�@�@�@�P@|�@\)@K�@;d@+@�@��@ȴ@�R@��@v�@$�@��@��@��@��@�h@�h@�@p�@?}@/@��@�/@��@�@��@z�@z�@j@9X@�@�m@ƨ@�@t�@dZ@dZ@S�@C�@o@��@��@�!@�!@�\@~�@M�@=q@J@��@��@�7@7L@ ��@ �u@ bN@ A�@ 1'@  �@   ?���?�|�?�;d?��?���?���?�v�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��#A���A��A��^A�I�A��A��DA���A�A���A���A�|�A�33A��uA�ȴA�x�A�bNA�XA�=qA�VA�ƨA�E�A�(�A�bA���A��DA�A�A�bA��jA�v�A�9XA��A��A�r�A�n�A�O�A�(�A���A�A�VA�VA���A��mA��A���A���A�ffA�v�A�p�A�$�A��FA�&�A��A�1'A�ffA�n�A���A���A�|�A��^A�  A��A��RA��PA� �A��RA���A���A�A��TA�hsA��A�;A{;dAy�Aw�PAv��Aut�Au%Asl�AqVAm?}AkoAiAg�wAfA�Ac��AbAa�wA`�!A_�7A]��A[�FAZ�`AY�AYK�AX��AX�+AW��AU��AS�-ARv�AQ�^AQO�AO�ANr�AN �AM�#AM\)AL5?AKS�AJ�!AJ1'AH�yAHJAG�7AGl�AG?}AF��AFn�AE��ADz�AC�
AChsAA�AAoA@�+A@M�A@�A?�mA?ƨA?��A?XA>�!A=?}A<��A;�A;�7A;7LA:�yA:v�A9�#A9/A8��A85?A7��A6��A5��A3�A3�PA3dZA37LA2��A2 �A1�A0v�A.bNA-K�A,�yA,jA+��A+dZA+�A*�A)�FA);dA(��A(v�A(VA'A';dA&�jA&E�A%��A%��A%�hA%dZA$�A$v�A#A"{A ��A|�A��AjA��A�AO�A�A(�A�RA��A��A�7AdZA%A�\A��A�-AĜA^5A�#A?}AZA �A`BA��AƨA33A�9AM�A��A\)A��AQ�AG�A
=qA	VAĜA��A�A�A�PAĜA  A�-A�HA�;AK�A�9AE�A�A�-A ��A bNA $�A  �A �@��;@�J@���@���@��+@�{@�X@��@�b@�P@�+@���@�@�V@��@��@�bN@�$�@�?}@�I�@�ȴ@��@�A�@��@�bN@�/@ݩ�@�"�@�|�@֗�@�M�@�{@�@���@պ^@Ձ@�O�@�Z@љ�@��y@�@�%@�1@��H@ʗ�@�@��/@�@�V@�(�@°!@��@��@���@��;@���@�x�@�  @���@�$�@���@�p�@���@��@��y@���@�ff@�-@�{@��T@��@��R@�/@���@��w@���@�t�@��@�@��h@�&�@��u@���@��@��R@�n�@���@���@�Z@��@�K�@�E�@�@��#@�@���@��D@��F@�
=@��y@���@��!@�ȴ@�ȴ@��!@��!@��!@���@�ff@�V@�=q@�5?@��@�G�@���@�b@���@�\)@�@�v�@��T@���@��D@�I�@�(�@�ƨ@�\)@�+@���@�$�@�`B@���@�j@��
@���@�33@��@���@���@�/@�V@���@���@�bN@� �@�ƨ@�C�@��y@�ȴ@���@�n�@���@�/@�Ĝ@�z�@�(�@��
@�@�x�@��/@��u@� �@��@���@��P@�
=@���@���@�^5@�M�@�5?@��T@�@��7@�O�@��`@�  @��@��P@�\)@���@�ȴ@��!@��!@���@�n�@�E�@�J@�@�hs@�?}@�%@��`@���@�Ĝ@���@�A�@l�@~�@}��@|�@|z�@|I�@{��@{�@{o@z=q@x��@w\)@v�y@v@u�-@u/@t��@t��@t��@t��@t��@s��@s"�@r=q@q��@q&�@p�@o�@o+@n�+@n{@n@m��@m�@l��@l9X@kt�@ko@j�!@j�\@j~�@j^5@i�@ix�@iX@i&�@h��@hĜ@h��@h�u@h�u@h�@hbN@hb@g�;@g�w@g�@g��@g��@g|�@g\)@f��@fȴ@fV@e�-@e�@e`B@eO�@e?}@d�@d9X@d�@c�
@c��@c33@b�\@bn�@b=q@b�@a��@a�@`��@`r�@`A�@`  @_��@_�@_��@_�P@_l�@_;d@_+@^��@^E�@^5?@^{@]�T@]@]�-@]p�@\��@\�@\Z@\�@[�m@[�@Z�H@ZM�@Y��@Y�#@Y�7@Yhs@YX@Y7L@Y�@X��@X��@X�u@W�@W��@Wl�@Vȴ@V��@V�R@V�+@V$�@U��@U�-@U�h@U`B@U/@T�@T9X@T1@S��@S33@R��@RM�@Q��@Q�^@Q�7@QX@Q7L@Q%@P�u@P1'@O��@O��@O;d@N��@Nȴ@N��@N�+@M�T@M?}@L�j@L�D@Lz�@L9X@L�@K�m@K��@KC�@J�H@J��@J�!@J��@J�\@J�\@J�\@Jn�@JM�@I��@I�7@I&�@H�`@H��@HA�@G�@G
=@F�+@F@E�-@E�h@E/@D�@Dj@D9X@C�@B�@B��@B��@B��@B�\@B-@A�#@A��@A��@A��@AG�@A�@@�@@  @?�w@?�P@?\)@?�@>��@>�R@>�+@>�+@>v�@>ff@>ff@>5?@>{@=�-@=�h@=O�@<�j@<j@<(�@;ƨ@;��@;S�@;@:�H@:�!@:~�@:�@9�7@9&�@8�`@8��@8A�@7�w@7;d@6�y@6��@5�T@5?}@4��@4�D@4�@3�F@3�@3"�@2~�@2n�@2n�@2n�@2n�@2^5@2M�@2=q@1��@1��@0�`@0��@0�`@0��@0��@0Ĝ@0r�@/��@/K�@/�@.�@.v�@.5?@.$�@-�@-��@-@-�-@-�h@-p�@-?}@-/@,��@,�@,�j@,z�@+�@+S�@+33@*�!@*�\@*�\@*^5@)�^@)7L@(�9@(bN@(1'@(1'@(1'@'��@'�@'�@'
=@'
=@'
=@'�@'�@&�@&5?@%��@%�h@%�h@%�@%`B@%/@$��@$�j@$�D@$I�@$�@#ƨ@#�@#C�@#"�@#o@#o@#@"�H@"��@"��@"^5@!hs@!7L@!�@ ��@ �9@  �@�@�P@�@��@�@�R@v�@{@�@�T@��@@�-@�@�@I�@�
@��@�@S�@33@"�@o@@@�@~�@^5@��@&�@�9@�u@r�@bN@Q�@A�@ �@b@�;@�w@��@l�@ȴ@�+@E�@$�@{@@�-@�h@p�@/@�@��@9X@1@1@��@33@"�@"�@��@~�@=q@��@�@�#@��@��@�7@x�@X@G�@&�@%@��@�`@��@�9@�9@��@Q�@ �@b@l�@+@��@ȴ@��@��@�+@v�@v�@V@V@E�@5?@@@@@�T@��@�-@�@O�@�@��@��@9X@ƨ@��@dZ@S�@"�@o@
�H@
�\@
-@	�#@	�^@	��@	�7@	G�@	&�@	%@��@�`@��@�9@�u@r�@A�@1'@ �@�@�@�@�P@|�@\)@K�@;d@+@�@��@ȴ@�R@��@v�@$�@��@��@��@��@�h@�h@�@p�@?}@/@��@�/@��@�@��@z�@z�@j@9X@�@�m@ƨ@�@t�@dZ@dZ@S�@C�@o@��@��@�!@�!@�\@~�@M�@=q@J@��@��@�7@7L@ ��@ �u@ bN@ A�@ 1'@  �@   ?���?�|�?�;d?��?���?���?�v�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B�hB��B��B��B��B��B��B�oB�7B�B�JB�VB�VB�JB�1B�B~�B�B�B|�Bz�Bv�Bt�Bp�Bn�BjBiyBdZBgmBffBaHBYBO�B6FB#�B
=B\B	7B�B�B��BȴB�LB��B�B��B��B�JBt�BW
BH�B5?B$�B'�B{B
=B
��BB
��B
�B
�fB
��B
ĜB
�3B
��B
��B
�uB
q�B
G�B
P�B
F�B
F�B
;dB
6FB
'�B
JB	�B	�yB	�B	�B	��B	�wB	�'B	�wB	�!B	��B	��B	�7B	�bB	�1B	�PB	�=B	�B	u�B	cTB	S�B	_;B	\)B	[#B	O�B	G�B	P�B	M�B	G�B	<jB	9XB	8RB	49B	+B	,B	/B	1'B	0!B	,B	(�B	$�B	�B	�B	�B	VB	bB	bB	oB	hB	bB	bB	PB		7B	B��B��B��B��B��B��B�B�B�yB�sB�ZB�;B�B��BƨB��B��B��B��BĜBB�LB��B�B�9B�-B�B�!B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�bB�VB�Bt�Bx�Bt�B}�B� Bz�B� Bz�Bv�Bp�BffBn�Br�Bt�Br�Bm�BhsB`BBQ�BVB`BB\)BZBT�BYBR�BN�BL�BO�BN�BN�BJ�BM�BI�BC�B<jB;dB9XBE�BE�B@�BA�BA�B8RB6FB:^B2-B.B49B33B49B5?B2-B.B/B5?B6FB33B,B�BuB�B#�B-B+B+B,B-B.B-B-B,B(�B$�B�B{B�B�B�B�B{BPBBB��B	7B%B!�B(�B)�B+B)�B(�B%�B!�B�BVBoB"�B"�B �B �B%�B#�B �B�B$�B%�B�B�B�B�B)�B+B,B+B1'B8RB9XB;dB9XB6FB6FB>wB>wB?}B>wB;dB1'B33B9XB?}BL�BM�BL�BI�BI�BN�BO�BN�BN�BR�BVBW
BT�BW
BZB^5B]/B^5BgmBhsBgmBcTBjBiyBm�Bu�Bv�Bx�By�Bx�Bw�Bx�Bx�Bx�Bv�Bx�Bx�Bx�Bu�Bt�Bv�B{�B~�B�B�B�B�%B�B�bB�oB�uB�uB�uB��B��B��B��B��B��B��B�B�B�B�B�B�'B�qB�}B��BBÖBĜBŢB��B��B��B��B��B�B�/B�NB�ZB�`B�TB�sB��B��B	  B	+B		7B		7B	DB	hB	{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	%�B	(�B	(�B	(�B	+B	.B	.B	.B	.B	/B	/B	0!B	33B	9XB	=qB	B�B	H�B	I�B	I�B	H�B	I�B	P�B	P�B	W
B	]/B	^5B	_;B	`BB	bNB	cTB	e`B	iyB	r�B	s�B	w�B	x�B	z�B	~�B	~�B	~�B	}�B	}�B	�B	�B	�1B	�DB	�PB	�bB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�'B	�9B	�9B	�?B	�LB	�RB	�XB	�XB	�XB	�XB	�XB	�dB	�jB	�qB	�qB	�qB	�qB	�qB	�qB	�wB	�}B	��B	ĜB	ŢB	ŢB	ŢB	ĜB	ŢB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�)B	�/B	�5B	�5B	�;B	�;B	�5B	�BB	�NB	�NB	�NB	�NB	�NB	�HB	�HB	�TB	�ZB	�`B	�`B	�`B	�fB	�mB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B	��B	��B
B
B
B
B
B
B
B
%B
%B
1B
1B
	7B
	7B
	7B
	7B
1B
1B
1B
	7B

=B

=B
DB
DB

=B
DB
PB
VB
bB
bB
bB
bB
oB
oB
oB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
 �B
 �B
!�B
!�B
"�B
#�B
#�B
#�B
#�B
"�B
$�B
%�B
%�B
%�B
%�B
&�B
(�B
(�B
'�B
)�B
,B
,B
,B
-B
.B
.B
.B
1'B
1'B
1'B
1'B
1'B
1'B
1'B
/B
1'B
0!B
33B
49B
33B
33B
33B
2-B
0!B
49B
5?B
5?B
5?B
6FB
7LB
7LB
7LB
7LB
7LB
7LB
8RB
7LB
8RB
8RB
8RB
8RB
7LB
6FB
:^B
:^B
9XB
;dB
;dB
;dB
9XB
;dB
<jB
=qB
>wB
?}B
?}B
=qB
>wB
A�B
A�B
A�B
A�B
A�B
@�B
?}B
>wB
@�B
B�B
C�B
B�B
B�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
D�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
D�B
C�B
G�B
G�B
G�B
G�B
F�B
H�B
H�B
H�B
I�B
J�B
J�B
I�B
J�B
K�B
K�B
K�B
K�B
K�B
J�B
J�B
I�B
L�B
M�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
N�B
M�B
N�B
N�B
M�B
P�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
R�B
S�B
T�B
VB
VB
VB
VB
VB
W
B
VB
W
B
W
B
W
B
YB
YB
XB
XB
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
\)B
[#B
\)B
\)B
\)B
]/B
]/B
]/B
]/B
^5B
]/B
]/B
]/B
^5B
\)B
^5B
_;B
_;B
`BB
`BB
aHB
`BB
aHB
aHB
aHB
aHB
aHB
`BB
aHB
aHB
aHB
aHB
aHB
aHB
`BB
aHB
aHB
aHB
`BB
`BB
aHB
cTB
cTB
cTB
cTB
dZB
cTB
cTB
cTB
cTB
dZB
e`B
e`B
dZB
ffB
ffB
ffB
ffB
ffB
ffB
ffB
ffB
ffB
gmB
gmB
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
jB
jB
iyB
iyB
iyB
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
l�B
l�B
l�B
m�B
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
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
r�B
s�B
s�B
t�B
t�B
s�B
s�B
t�B
t�B
t�B
u�B
t�B
u�B
u�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�=B�_B��B��B��B�B�B�B�EB��B�9B��B��B�[B��B��B��B��B��B��B��B��B� B�SB�[B}�B{dBwfBu?Bq[Bo5BkBj0Be,Bg�Bf�Ba�BY�BQB8�B&�B�B�B
rG�O�B��B͹B��B�rB��B��B��B�
B�<BxRB[	BK^B8�B'B(�BmBB
�BoB
�qB
��B
�B
��B
�B
��B
�qB
�BB
��B
vFB
MB
S@B
H�B
G�B
<�B
7fB
*eB
�B	�B	�B	ܒB	��B	��B	�;B	�3B	�.B	��B	��B	��B	�xB	��B	��B	��B	��B	��B	w2B	e�B	VmB	`�B	]IB	\B	Q�B	IB	QhB	NVB	H�B	=�B	:xB	9XB	5%B	,�B	-B	/�B	1vB	0�B	,�B	)�B	%�B	B	�B	�B	.B	4B	B	�B	�B	�B	�B	�B		�B	3B��B��B��B�RB�RB�?B�[B�cB�KB�B�FB�'B�7BӏB��B�TB�FB�TB̈́BżB�{B�$B�kB�iB��B��B��B��B��B��B�B��B��B�`B�LB��B�jB�WB�WB�=B��B��B��B�4B�(B�tBwBz�Bv+B~�B��B{�B�OB{�Bw�Bq�Bh>Bo�BsBuBsBnIBi_Ba�BT�BW?B`�B\�B[	BV9BY�BTBO�BNBP�BO�BO�BK�BNVBJ�BD�B=�B<�B:�BFBFBAoBA�BBB9rB7fB:�B3�B/iB5B4B4�B5�B2�B/B/�B5�B6zB3�B,�B;B�B5B$�B-]B+�B+�B,�B-wB.cB-]B-wB,qB)_B%zB�B�B \B�B�B�B�B�BBMB�6B
�BfB"4B)DB*0B+B*KB)*B&2B"NB�BHB�B#nB#�B!�B!�B&LB$�B!�B~B%�B&�B�B!B/B!-B*�B+�B,�B,B2B8�B9�B;�B9�B7B6�B>�B>�B?�B>�B;�B2|B4TB:^B@OBL�BNBMBJXBJXBO\BPHBO\BO�BS[BVmBWsBU�BW�BZ�B^�B]�B^�Bg�Bh�Bg�Bd@Bj�BjBnBu�BwBx�By�Bx�Bw�Bx�Bx�Bx�Bv�Bx�By	By	Bv+BuZBwLB|PBcB�[B�{B��B��B��B��B��B��B��B��B��B��B�?B�1B�!B�@B�LB�6B�kB�cB�}B��B��B��B��B��B��B��B��B�B�B��B� B�HB�}B�eB�~B�B�B��B�&B�yB�%B�<B	 iB	_B		lB		�B	�B	�B	�B	�B	�B	�B	B	�B	�B	B	B	OB	&B	)*B	)*B	)DB	+6B	./B	./B	./B	.IB	/OB	/OB	0oB	3�B	9�B	=�B	B�B	H�B	I�B	I�B	IB	J#B	Q4B	QhB	WYB	]IB	^OB	_�B	`�B	b�B	c�B	e�B	jB	r�B	tB	w�B	y	B	{B	B	B	B	~(B	~BB	�UB	�{B	�fB	�xB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	� B	�B	�*B	�B	�)B	�IB	�cB	�[B	�TB	�TB	�ZB	�fB	�lB	�rB	�XB	��B	�rB	�rB	��B	��B	��B	��B	�qB	��B	��B	��B	��B	��B	��B	��B	żB	żB	żB	��B	��B	��B	��B	��B	��B	�B	�B	�&B	�B	�&B	�@B	�B	�?B	�1B	�1B	�7B	�CB	�IB	�OB	�OB	�pB	�pB	�jB	�\B	�hB	�hB	�hB	�hB	�hB	�bB	�B	�nB	�tB	�zB	�zB	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B
 B
 B
 4G�O�B	�HB
[B
3B
9B
MB
9B
9B
9B
?B
YB
1B
KB
	RB
	7B
	7B
	7B
KB
KB
fB
	lB

XB

XB
^B
xB

�B
xB
�B
�B
}B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
 �B
 �B
!�B
!�B
"�B
#�B
#�B
#�B
$B
# B
%B
%�B
%�B
&B
&B
'B
)*B
)*B
(>B
*0B
,"B
,=B
,=B
-)B
./B
.IB
.IB
1'B
1'B
1'B
1'B
1AB
1AB
1[G�O�B
1AB
0UB
33B
49B
3MB
33B
3MB
2aG�O�B
4TB
5ZB
5ZB
5tB
6`B
7fB
7fB
7fB
7LB
7fB
7fB
8RB
7fB
8lB
8�B
8�B
8lB
7�B
6�B
:xB
:xB
9�B
;B
;�B
;�G�O�B
;�B
<�B
=�B
>�B
?}B
?�G�O�B
>�B
A�B
A�B
A�B
A�B
A�B
@�B
?�B
>�B
@�B
B�B
C�B
B�B
B�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
D�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
D�B
C�B
G�B
G�B
G�B
G�B
F�B
H�B
IB
H�B
I�B
J�B
J�B
I�B
J�B
K�B
K�B
K�B
K�B
K�B
J�B
J�B
J	B
MB
M�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
OB
NB
N�B
OB
N"B
Q B
SB
SB
R�B
S�B
S�B
TB
T,B
TB
TB
TB
TB
S@B
TB
UB
VB
VB
VB
V9B
VB
W$B
V9B
W$B
W$B
W?B
Y1B
Y1B
XEB
XEB
Y1B
YB
X+B
X+B
YKB
Z7B
[=B
[#B
[#B
[=B
[=B
\CB
[=B
\)B
\CB
\CB
]/B
]/B
]/B
]IB
^5B
]IB
]IB
]IB
^5G�O�B
^OB
_VB
_pB
`\B
`BB
aHB
`\B
aHB
abB
aHB
aHB
aHB
`\B
aHB
aHB
aHB
abB
aHB
abB
`\B
abB
abB
abB
`vB
`�B
abB
cnB
cnB
cnB
c�B
dtB
cnB
cnB
cnB
c�B
dtB
e`B
e�B
dtB
f�B
f�B
ffB
f�B
ffB
f�B
f�B
f�B
f�B
gmB
g�B
g�B
h�B
iyB
i�B
iyB
i�B
iyB
iyB
iyB
iyB
i�B
i�B
jB
jB
i�B
i�B
i�B
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
l�B
l�B
l�B
m�B
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
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
r�B
s�B
s�B
t�B
t�B
s�B
s�B
t�B
t�B
t�B
u�B
t�B
u�B
u�1111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111411111111111111111111111114111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.02(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201804120035592018041200355920180412003559201806221240112018062212401120180622124011201804271405312018042714053120180427140531  JA  ARFMdecpA19c                                                                20180408093516  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180408003522  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180408003525  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180408003525  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180408003526  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180408003526  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180408003526  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180408003526  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180408003528  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180408003528                      G�O�G�O�G�O�                JA  ARUP                                                                        20180408005648                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180408153748  CV  JULD            G�O�G�O�F�ˤ                JM  ARSQJMQC2.0                                                                 20180409000000  CF  PSAL_ADJUSTED_QCC  Dۀ G�O�                JM  ARCAJMQC2.0                                                                 20180411153559  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180411153559  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180427050531  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622034011  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115121516                      G�O�G�O�G�O�                