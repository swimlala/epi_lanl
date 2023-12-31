CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-09-23T00:35:36Z creation;2018-09-23T00:35:41Z conversion to V3.1;2019-12-19T07:28:13Z update;     
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
_FillValue                    �<Argo profile    3.1 1.2 19500101000000  20180923003536  20200116231516  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL              A   JA  I2_0577_284                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @؃t��� 1   @؃u�d� @4�*�0��dYP��{�1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�ff@�33A   A   A@  A^ffA�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dmy�Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�@\@�\)A�A?�A^{A�A��
A��
A��
A��
A��
A��
A��
A��
B�B�B�B�B'�B/�B7�B?�BG�BO�BW�B_�Bg�Bo�Bw�B�B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qD ~�D ��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D	~�D	��D
~�D
��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D ~�D ��D!~�D!��D"~�D"��D#~�D#��D$~�D$��D%~�D%��D&~�D&��D'~�D'��D(~�D(��D)~�D)��D*~�D*��D+~�D+��D,~�D,��D-~�D-��D.~�D.��D/~�D/��D0~�D0��D1~�D1��D2~�D2��D3~�D3��D4~�D4��D5~�D5��D6~�D6��D7~�D7��D8~�D8��D9~�D9��D:~�D:��D;~�D;��D<~�D<��D=~�D=��D>~�D>��D?~�D?��D@~�D@��DA~�DA��DB~�DB��DC~�DC��DD~�DD��DE~�DE��DF~�DF��DG~�DG��DH~�DH��DI~�DI��DJ~�DJ��DK~�DK��DL~�DL��DM~�DM��DN~�DN��DO~�DO��DP~�DP��DQ~�DQ��DR~�DR��DS~�DS��DT~�DT��DU~�DU��DV~�DV��DW~�DW��DX~�DX��DY~�DY��DZ~�DZ��D[~�D[��D\~�D\��D]~�D]��D^~�D^��D_~�D_��D`~�D`��Da~�Da��Db~�Db��Dc~�Dc��Dd~�Dd��De~�De��Df~�Df��Dg~�Dg��Dh~�Dh��Di~�Di��Dj~�Dj��Dk~�Dk��Dl~�Dl��DmxRDm��Dn~�Dn��Do~�Do��Dp~�Dp��Dq~�Dq��Dr~�Dr��Ds~�Ds��Dt~�Dt��Du~�Du��Dv~�Dv��Dw~�Dw��Dx~�Dx��Dy~�Dy��Dz~�Dz��D{~�D{��D|~�D|��D}~�D}��D~~�D~��D~�D��D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D���D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D¿\D��\D�?\D�\Dÿ\D��\D�?\D�\DĿ\D��\D�?\D�\Dſ\D��\D�?\D�\Dƿ\D��\D�?\D�\Dǿ\D��\D�?\D�\Dȿ\D��\D�?\D�\Dɿ\D��\D�?\D�\Dʿ\D��\D�?\D�\D˿\D��\D�?\D�\D̿\D��\D�?\D�\DͿ\D��\D�?\D�\Dο\D��\D�?\D�\DϿ\D��\D�?\D�\Dп\D��\D�?\D�\Dѿ\D��\D�?\D�\Dҿ\D��\D�?\D�\Dӿ\D��\D�?\D�\DԿ\D��\D�?\D�\Dտ\D��\D�?\D�\Dֿ\D��\D�?\D�\D׿\D��\D�?\D�\Dؿ\D��\D�?\D�\Dٿ\D��\D�?\D�\Dڿ\D��\D�?\D�\Dۿ\D��\D�?\D�\Dܿ\D��\D�?\D�\Dݿ\D��\D�?\D�\D޿\D��\D�?\D�\D߿\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D��\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�B�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�K�A�I�A�G�A�G�A�E�A�C�A�C�A�A�A�A�A�;dA�5?A�-A�&�A� �A�A���A��mA���A�ƨAݣ�A݃A�r�A�^5A�;dA�bA���A�33AׅAӰ!AЃA�ZA��A�=qA���Ař�AÅA�A�A���A�oA��A���A���A��FA�7LA�=qA�VA�E�A�;dA�-A��;A�1A��A���A�ĜA���A�A�A�dZA�v�A�33A�E�A���A�?}A��#A�7LA��A�ZA���A�?}A���A�VA���A�oA�dZA�+A�VA�1'A���A�C�A��A� �A�ĜA�%A���A��A���A�
=A��-A��A�v�A�K�A���A���A���A���A���A��A��
A�l�A��HA�?}A���A�A��A��!A��jA��;A��A�A���A�|�A��A�XA�ZA��A}�A}|�A|�A|ZAz��Ay;dAx �Av$�AuG�At1'Aq�hAljAi�Ah�Ah�+Ag�Ag7LAf��Ae��Ab��A_�A^�jA[AW�TAVbAT�AShsAQ/AM�7AL�AJr�AIl�AHjAF5?AD�AChsABȴABVA?�
A=�-A<-A9\)A6�\A4�A3�hA2�\A1;dA. �A,E�A,9XA+p�A*��A)��A(�A'G�A&�A%33A#+A"ZA"A!��A ȴA��A�-AK�A�AXAK�Ar�A�HA��A7LA��A{AhsAz�A�A��AXA%A��AZA1'A?}A��A�A
�DA
1A�A-A�\A�AZA��AS�A�AE�A�#A�#A�A�PAbNA��A��A�A ��A �\A �@��@�7L@��R@��@�I�@�bN@�@�I�@�"�@�j@�F@�S�@�-@���@�5?@�p�@��@�Z@��@�7@��@�;d@�~�@��@���@ܓu@ۅ@�%@׍P@׍P@�o@�5?@թ�@ՙ�@�&�@ՙ�@���@�b@ӥ�@�+@��@мj@��@��#@���@�Ĝ@��`@���@���@̓u@̃@��@��@ɺ^@Ɂ@�O�@�V@�V@�{@�@�&�@�I�@��@�$�@��@�z�@�bN@���@�|�@þw@���@���@��@�X@��F@��@�1'@�K�@�v�@��T@�hs@��j@�z�@�j@�bN@�1'@���@��y@���@��@�@��y@���@�=q@�5?@�hs@���@��9@��u@�1'@�Q�@�A�@�  @�dZ@��y@�{@�V@�z�@��m@�|�@�K�@��@�@��y@���@�=q@�-@�5?@�E�@�E�@�{@���@�?}@��/@��D@�1@��;@�ƨ@��w@��w@��@�|�@�+@�
=@��H@��R@�n�@�V@��T@�x�@�`B@�7L@�&�@�V@��j@��@�  @�\)@��@�@��y@���@���@�~�@�M�@�J@���@�hs@�7L@��/@�Ĝ@��@���@�bN@�1'@�1@��;@���@��w@���@�;d@���@���@���@�v�@�E�@��@���@���@�@���@�%@��`@��D@�bN@�I�@�1@�ƨ@���@�@��!@���@��+@��@��#@���@��-@��h@�hs@��@��@���@�Z@�(�@�1@��;@��F@�|�@�;d@���@�ff@�@��@��@��@���@���@��T@��T@���@�G�@��/@��9@��@�(�@�l�@�ȴ@���@��+@�v�@�ff@�ff@�v�@�{@�5?@��@�{@�@��h@��@�bN@�1@�  @��;@���@��P@�;d@�|�@���@�;d@�
=@�o@�o@��!@��h@�hs@�p�@���@��-@���@���@��@�`B@�X@�V@�r�@�  @��P@�dZ@�l�@�S�@�|�@��@��P@�"�@��+@�p�@��@�V@�%@��@��D@�1@��F@��P@�l�@�"�@��@��+@�E�@�5?@��@���@���@�p�@�G�@��9@��@�I�@� �@~��@~�R@~��@~ff@~5?@}@}�-@}��@}V@|�/@|��@|�@|�D@|�D@|�D@|��@|��@{��@{33@z�@y�@y��@yx�@x�`@xĜ@x�@x1'@xb@w��@wK�@v�@vv�@v{@u@up�@uV@t1@s��@s�@sC�@r�@r��@r�\@r�\@r~�@r=q@qX@p��@pQ�@o�@o��@o�w@o��@o;d@o
=@o
=@n��@n��@nv�@m@mO�@l��@l�j@l(�@kC�@j~�@j�@i�@i�@i�#@ihs@hĜ@hbN@g�@g�w@g\)@g�@f��@f�R@fv�@fV@fV@f$�@f{@e�@e�@e@e�h@e�@d�j@dI�@cƨ@cC�@b�!@b��@b^5@a�@a��@a7L@a%@`��@`r�@`  @_��@_��@_l�@_;d@^�y@^ȴ@^�R@^�+@^E�@]�@^@]@]�h@\�@\j@\�@[�m@[�m@[��@Z��@Z^5@Y��@YX@Y%@Y%@XĜ@X �@Wl�@W
=@V�@V��@V5?@V@T��@T��@T�D@TZ@T�@S�m@S��@SdZ@S33@R��@R^5@R=q@Q�7@Q%@P��@P��@P�9@PQ�@Pb@P  @P  @PA�@Pr�@P  @O�@N�+@NV@M�T@M��@M�h@M�@M�@M`B@MV@L�j@L�@L9X@L�@L1@K�F@K�@KdZ@Ko@J^5@J=q@JJ@I�@I�#@Ix�@H��@H��@H�@HQ�@Hb@G+@F�R@Fff@F{@E�-@Ep�@EO�@EV@D�j@D9X@D1@C�F@C��@C�@CS�@C"�@B��@B��@Bn�@BM�@B�@A�#@A��@A7L@@��@@��@@Ĝ@@�u@@A�@@1'@?�@?�@?\)@?�@?
=@>ȴ@>�+@>5?@=�T@=�h@=O�@<�/@<�j@<�j@<�@<��@<9X@;��@;C�@:�@:��@:-@9��@9��@9X@9%@8Ĝ@8Q�@7�;@7�@7l�@7;d@7
=@6�@6�R@6�+@65?@5�T@5�h@5O�@5O�@4�/@4�D@4j@3�m@3��@3�@3C�@3@2��@2~�@2M�@2�@2J@1�@1�7@1G�@1%@0bN@0 �@0b@/�;@/�w@/��@/�P@/K�@/�@.�@.��@.E�@.{@-�T@-�-@-p�@-O�@-V@,�/@,�j@,�D@,I�@,1@+ƨ@+S�@+@*��@*n�@*-@)��@)��@)x�@)X@)&�@(��@(�9@(bN@(1'@( �@'�;@'�P@'l�@';d@'
=@&ȴ@&��@&ff@%�@%��@%�h@%/@$�@$��@$I�@#�
@#�F@#��@#�@#dZ@#"�@#o@"�@"�!@"��@"~�@"^5@"M�@"�@!��@!�#@!�7@!&�@ �`@ �9@ ��@ bN@��@+@
=@�y@ȴ@��@�+@v�@ff@V@V@V@5?@@@��@�h@O�@V@��@��@I�@(�@(�@1@t�@o@@@@�H@�!@M�@M�@�#@�7@hs@G�@7L@&�@&�@&�@%@�`@��@�@1'@�;@�@�P@l�@;d@�@v�@�T@��@�@O�@?}@�/@�j@z�@1@��@�m@�m@�
@ƨ@�@C�@"�@��@~�@M�@=q@=q@-@�@J@�@��@&�@��@Ĝ@�u@Q�@1'@  @�;@�P@l�@\)@;d@
=@�y@�@ȴ@��@ff@$�@�@��@p�@O�@/@V@�/@�@z�@Z@Z@Z@(�@(�@�@1@��@��@t�@t�@S�@o@
��@
�!1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�K�A�I�A�G�A�G�A�E�A�C�A�C�A�A�A�A�A�;dA�5?A�-A�&�A� �A�A���A��mA���A�ƨAݣ�A݃A�r�A�^5A�;dA�bA���A�33AׅAӰ!AЃA�ZA��A�=qA���Ař�AÅA�A�A���A�oA��A���A���A��FA�7LA�=qA�VA�E�A�;dA�-A��;A�1A��A���A�ĜA���A�A�A�dZA�v�A�33A�E�A���A�?}A��#A�7LA��A�ZA���A�?}A���A�VA���A�oA�dZA�+A�VA�1'A���A�C�A��A� �A�ĜA�%A���A��A���A�
=A��-A��A�v�A�K�A���A���A���A���A���A��A��
A�l�A��HA�?}A���A�A��A��!A��jA��;A��A�A���A�|�A��A�XA�ZA��A}�A}|�A|�A|ZAz��Ay;dAx �Av$�AuG�At1'Aq�hAljAi�Ah�Ah�+Ag�Ag7LAf��Ae��Ab��A_�A^�jA[AW�TAVbAT�AShsAQ/AM�7AL�AJr�AIl�AHjAF5?AD�AChsABȴABVA?�
A=�-A<-A9\)A6�\A4�A3�hA2�\A1;dA. �A,E�A,9XA+p�A*��A)��A(�A'G�A&�A%33A#+A"ZA"A!��A ȴA��A�-AK�A�AXAK�Ar�A�HA��A7LA��A{AhsAz�A�A��AXA%A��AZA1'A?}A��A�A
�DA
1A�A-A�\A�AZA��AS�A�AE�A�#A�#A�A�PAbNA��A��A�A ��A �\A �@��@�7L@��R@��@�I�@�bN@�@�I�@�"�@�j@�F@�S�@�-@���@�5?@�p�@��@�Z@��@�7@��@�;d@�~�@��@���@ܓu@ۅ@�%@׍P@׍P@�o@�5?@թ�@ՙ�@�&�@ՙ�@���@�b@ӥ�@�+@��@мj@��@��#@���@�Ĝ@��`@���@���@̓u@̃@��@��@ɺ^@Ɂ@�O�@�V@�V@�{@�@�&�@�I�@��@�$�@��@�z�@�bN@���@�|�@þw@���@���@��@�X@��F@��@�1'@�K�@�v�@��T@�hs@��j@�z�@�j@�bN@�1'@���@��y@���@��@�@��y@���@�=q@�5?@�hs@���@��9@��u@�1'@�Q�@�A�@�  @�dZ@��y@�{@�V@�z�@��m@�|�@�K�@��@�@��y@���@�=q@�-@�5?@�E�@�E�@�{@���@�?}@��/@��D@�1@��;@�ƨ@��w@��w@��@�|�@�+@�
=@��H@��R@�n�@�V@��T@�x�@�`B@�7L@�&�@�V@��j@��@�  @�\)@��@�@��y@���@���@�~�@�M�@�J@���@�hs@�7L@��/@�Ĝ@��@���@�bN@�1'@�1@��;@���@��w@���@�;d@���@���@���@�v�@�E�@��@���@���@�@���@�%@��`@��D@�bN@�I�@�1@�ƨ@���@�@��!@���@��+@��@��#@���@��-@��h@�hs@��@��@���@�Z@�(�@�1@��;@��F@�|�@�;d@���@�ff@�@��@��@��@���@���@��T@��T@���@�G�@��/@��9@��@�(�@�l�@�ȴ@���@��+@�v�@�ff@�ff@�v�@�{@�5?@��@�{@�@��h@��@�bN@�1@�  @��;@���@��P@�;d@�|�@���@�;d@�
=@�o@�o@��!@��h@�hs@�p�@���@��-@���@���@��@�`B@�X@�V@�r�@�  @��P@�dZ@�l�@�S�@�|�@��@��P@�"�@��+@�p�@��@�V@�%@��@��D@�1@��F@��P@�l�@�"�@��@��+@�E�@�5?@��@���@���@�p�@�G�@��9@��@�I�@� �@~��@~�R@~��@~ff@~5?@}@}�-@}��@}V@|�/@|��@|�@|�D@|�D@|�D@|��@|��@{��@{33@z�@y�@y��@yx�@x�`@xĜ@x�@x1'@xb@w��@wK�@v�@vv�@v{@u@up�@uV@t1@s��@s�@sC�@r�@r��@r�\@r�\@r~�@r=q@qX@p��@pQ�@o�@o��@o�w@o��@o;d@o
=@o
=@n��@n��@nv�@m@mO�@l��@l�j@l(�@kC�@j~�@j�@i�@i�@i�#@ihs@hĜ@hbN@g�@g�w@g\)@g�@f��@f�R@fv�@fV@fV@f$�@f{@e�@e�@e@e�h@e�@d�j@dI�@cƨ@cC�@b�!@b��@b^5@a�@a��@a7L@a%@`��@`r�@`  @_��@_��@_l�@_;d@^�y@^ȴ@^�R@^�+@^E�@]�@^@]@]�h@\�@\j@\�@[�m@[�m@[��@Z��@Z^5@Y��@YX@Y%@Y%@XĜ@X �@Wl�@W
=@V�@V��@V5?@V@T��@T��@T�D@TZ@T�@S�m@S��@SdZ@S33@R��@R^5@R=q@Q�7@Q%@P��@P��@P�9@PQ�@Pb@P  @P  @PA�@Pr�@P  @O�@N�+@NV@M�T@M��@M�h@M�@M�@M`B@MV@L�j@L�@L9X@L�@L1@K�F@K�@KdZ@Ko@J^5@J=q@JJ@I�@I�#@Ix�@H��@H��@H�@HQ�@Hb@G+@F�R@Fff@F{@E�-@Ep�@EO�@EV@D�j@D9X@D1@C�F@C��@C�@CS�@C"�@B��@B��@Bn�@BM�@B�@A�#@A��@A7L@@��@@��@@Ĝ@@�u@@A�@@1'@?�@?�@?\)@?�@?
=@>ȴ@>�+@>5?@=�T@=�h@=O�@<�/@<�j@<�j@<�@<��@<9X@;��@;C�@:�@:��@:-@9��@9��@9X@9%@8Ĝ@8Q�@7�;@7�@7l�@7;d@7
=@6�@6�R@6�+@65?@5�T@5�h@5O�@5O�@4�/@4�D@4j@3�m@3��@3�@3C�@3@2��@2~�@2M�@2�@2J@1�@1�7@1G�@1%@0bN@0 �@0b@/�;@/�w@/��@/�P@/K�@/�@.�@.��@.E�@.{@-�T@-�-@-p�@-O�@-V@,�/@,�j@,�D@,I�@,1@+ƨ@+S�@+@*��@*n�@*-@)��@)��@)x�@)X@)&�@(��@(�9@(bN@(1'@( �@'�;@'�P@'l�@';d@'
=@&ȴ@&��@&ff@%�@%��@%�h@%/@$�@$��@$I�@#�
@#�F@#��@#�@#dZ@#"�@#o@"�@"�!@"��@"~�@"^5@"M�@"�@!��@!�#@!�7@!&�@ �`@ �9@ ��@ bN@��@+@
=@�y@ȴ@��@�+@v�@ff@V@V@V@5?@@@��@�h@O�@V@��@��@I�@(�@(�@1@t�@o@@@@�H@�!@M�@M�@�#@�7@hs@G�@7L@&�@&�@&�@%@�`@��@�@1'@�;@�@�P@l�@;d@�@v�@�T@��@�@O�@?}@�/@�j@z�@1@��@�m@�m@�
@ƨ@�@C�@"�@��@~�@M�@=q@=q@-@�@J@�@��@&�@��@Ĝ@�u@Q�@1'@  @�;@�P@l�@\)@;d@
=@�y@�@ȴ@��@ff@$�@�@��@p�@O�@/@V@�/@�@z�@Z@Z@Z@(�@(�@�@1@��@��@t�@t�@S�@o@
��@
�!1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
� B
�+B
��B
ÖB
��B�B�B
�B(�BgmB�B�`B+BuBbBB��B�BuB�B �B+B9XBN�BO�B\)B~�B�B}�Bu�BiyBl�BZBG�BO�BVB_;Bk�Br�Be`BJ�BdZBbNB]/BM�BVBL�BO�B?}B+B6FB!�B{B��B�BB�/B��BÖB��B�JB�B�DBcTBe`B`BBW
B]/B^5B`BBgmBW
B7LB�B
��B  BB
�fB
�B
�B
��B
��B
�dB
�RB
�B
��B
�+B
u�B
k�B
e`B
N�B
D�B
R�B
8RB
�B
!�B
;dB
8RB
,B
�B

=B
1B	�B	�B	�5B	�^B	z�B	��B	�RB	�jB	�9B	��B	��B	�bB	p�B	[#B	W
B	9XB	�B	49B	5?B	�B	B��B�B�TB�fB�5BǮB��B��B��BɺB��B��B��B�B�B}�B�B��B��B��B��B�!B�wB�jB�9B�!B��B��B��B�uB�B�XB�^B�9B�XB�jB��B�?B�3B��B{�B�B�B�B�B�7B�B�7B�PB�PB�hB��B��B��B�B�DB��B��B��B�B��B�-BĜB�FB�FBɺB�
B��B��B��B�#B�)B�#B��B��BȴBŢB�qBB�}B�9B��B��B�B�=By�B�JB�\B�\B�B�+B�1B� Bq�Bw�B~�B�B�B{�B~�B�B�+B�\B��B��B��B��B��B��B�B�B�B�-B��BÖB��B��B��B�#B�#B�B��B��B�B�B�HB�fB�mB�fB�ZB�`B�ZB�NB�mB�B�B��B	  B��B��B��B��B��B��B��B	B	%B	B	B	B��B�B�B�B��B�sB��B��B��B��B��B	B	+B	
=B	
=B	1B	B	B��B��B	JB	�B	�B	�B	�B	�B	�B	�B	�B	 �B	2-B	33B	1'B	/B	0!B	0!B	1'B	6FB	9XB	>wB	C�B	E�B	G�B	G�B	F�B	G�B	O�B	R�B	T�B	VB	W
B	VB	S�B	VB	\)B	`BB	gmB	jB	k�B	k�B	l�B	p�B	p�B	u�B	w�B	w�B	x�B	z�B	z�B	{�B	�B	�B	�B	�B	~�B	~�B	|�B	� B	�+B	�=B	�DB	�JB	�VB	�bB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�FB	�FB	�FB	�?B	�XB	�XB	�^B	�jB	�dB	�dB	�}B	�jB	��B	ĜB	ĜB	ÖB	ƨB	ɺB	ɺB	ɺB	ȴB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�)B	�5B	�BB	�;B	�5B	�;B	�;B	�HB	�HB	�BB	�)B	�)B	�HB	�TB	�ZB	�fB	�mB	�sB	�fB	�B	�B	�B	�B	�sB	�fB	�fB	�B	�B	�B	�B	�B	�B	��B	��B	�B	�B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
  B
  B	��B
  B
B
B
B
B
B
B	��B
B
B
B
B
B
1B
1B
	7B
1B

=B

=B
	7B
DB
JB
PB
PB
VB
VB
\B
VB
DB
PB
VB
JB
bB
bB
bB
uB
{B
{B
�B
�B
{B
{B
{B
�B
�B
�B
�B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
�B
�B
 �B
!�B
 �B
�B
�B
 �B
!�B
"�B
"�B
"�B
#�B
#�B
#�B
$�B
$�B
$�B
$�B
%�B
%�B
$�B
#�B
#�B
#�B
$�B
$�B
$�B
%�B
'�B
'�B
&�B
&�B
'�B
(�B
(�B
'�B
(�B
)�B
+B
+B
+B
)�B
+B
,B
+B
+B
+B
-B
,B
,B
)�B
-B
-B
.B
/B
-B
+B
-B
/B
.B
/B
0!B
/B
-B
-B
/B
0!B
0!B
/B
0!B
.B
1'B
33B
2-B
2-B
2-B
33B
2-B
33B
2-B
2-B
33B
2-B
33B
5?B
6FB
8RB
8RB
8RB
9XB
:^B
:^B
;dB
8RB
6FB
7LB
9XB
9XB
:^B
<jB
<jB
=qB
<jB
<jB
<jB
>wB
=qB
>wB
?}B
>wB
?}B
?}B
?}B
=qB
@�B
@�B
A�B
@�B
?}B
@�B
A�B
B�B
A�B
A�B
>wB
B�B
C�B
C�B
C�B
D�B
E�B
D�B
D�B
C�B
E�B
E�B
F�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
F�B
G�B
F�B
G�B
H�B
H�B
G�B
G�B
H�B
H�B
H�B
H�B
H�B
J�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
L�B
M�B
L�B
K�B
K�B
J�B
L�B
L�B
L�B
M�B
N�B
M�B
N�B
N�B
M�B
M�B
M�B
N�B
N�B
O�B
O�B
P�B
P�B
P�B
O�B
O�B
P�B
P�B
Q�B
P�B
P�B
Q�B
P�B
R�B
R�B
R�B
R�B
S�B
R�B
S�B
S�B
T�B
T�B
S�B
S�B
T�B
S�B
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
W
B
XB
XB
XB
XB
YB
YB
YB
ZB
ZB
YB
ZB
ZB
YB
[#B
[#B
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
^5B
_;B
^5B
^5B
_;B
_;B
_;B
_;B
`BB
_;B
_;B
`BB
`BB
_;B
`BB
`BB
aHB
aHB
cTB
cTB
cTB
cTB
bNB
dZB
dZB
cTB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
cTB
cTB
dZB
e`B
e`B
dZB
bNB
e`B
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
hsB
hsB
gmB
hsB
hsB
hsB
hsB
iyB
hsB
gmB
jB
iyB
iyB
gmB
iyB
k�B
k�B
k�B
jB
jB
jB
k�B
jB
jB
l�B
l�B
l�B
l�B
m�B
l�B
l�B
l�B
l�B
l�B
k�B
k�B
l�B
m�B
m�B
l�B
k�B
l�B
l�B
n�B
o�B
n�B
o�B
n�B
o�B
o�B
o�B
p�B
q�B
q�B
q�B
p�B
p�B
p�B
q�B
p�B
q�B
r�B
r�B
s�B
s�B
s�B
r�B
r�B
q�B
q�B
r�B
s�B
s�B
s�B
t�B
t�B
t�B
s�B
t�B
u�B
u�B
t�B
u�B
u�B
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
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
v�B
w�B
x�B
w�B
w�B
w�B
y�B
y�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
�B
�B
�B
�B
�B
�B
�B
�B
�'B
�-B
�-B
�-B
�3B
�AB
�-B
�-B
�GB
�-B
�UB
�OB
�zB
�B
�B
��B�B�B
��B0�BncB�~B�DB
�B�B�B�B�B��B2B 'B"�B-)B;JBPBRB]�B~�B�-B~]Bv�BkQBm�B\)BI�BQ�BW�Ba-BmCBs�Bg�BM�Be,BcnB^�BPbBWYBNpBP�BAUB-�B6�B#�BSB��B�B��B�NB�B�B��B�EB�PBg�Bg�BbhBX�B^B_�Ba�Bg�BX_B:DB�B
�.B[B�B
��B
��B
�KB
ϑB
ՁB
�qB
��B
�oB
��B
��B
x�B
m�B
g�B
Q�B
F�B
S�B
:�B
B
#�B
;�B
9	B
-)B
�B
B
	�B	�B	��B	�\B	�B	�UB	�ZB	�$B	�B	�B	�B	��B	�TB	tB	^�B	YKB	=�B	#nB	6FB	6�B	 B	3B�uB�%B�zB��B��BʌB��B�FB�B��B�>B��B�!B��B�fB�;B��B��B�B�1B��B��B��B��B��B��B��B�B��B�B�B��B�B�tB�^B�B�UB��B��B��BcB��B�SB�?B�B�	B�3B�XB��B�B��B�
B�WB��B��B��B��B�OB�,B��B�B��B��B�B�2B�7BּBңBҽBуB�=B�]B�BуB�B��B��B��B�-B�OB�tB�eB��B�+B��B|B��B�\B�HB�uB��B��B�BsMBx�B�B�mB��B|�B� B��B�B��B��B��B��B��B�/B��B�B��B��B��B��B�B��B�~BуB�qBۦB�B�B�9B��BںB�bB�fB�mB�B�B�B��B�:B�$B��B��B�jB	 B�.B�BB��B��B��B��B��B	GB	YB	mB	{B	9B��B�'B�B��B��B�KB�fB��B�fB�RB�}B	�B	_B	
rB	
XB	�B	�B	�B��B�PB	�B	�B	�B	�B	�B	+B	�B	�B	�B	!B	2-B	3MB	1vB	/�B	0�B	0�B	1�B	6�B	9�B	>�B	C�B	E�B	G�B	G�B	GB	HB	O�B	R�B	UB	VB	W?B	VSB	T{B	VmB	\xB	`�B	g�B	j�B	k�B	k�B	l�B	p�B	p�B	u�B	xB	xB	y	B	{B	{0B	|6B	� B	� B	� B	� B	HB	.B	}�B	�iB	�zB	�XB	�^B	�~B	�pB	�}B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�B	�B	�
B	�8B	�*B	�B	�6B	�=B	�=B	�cB	�iB	�+B	�`B	�zB	��B	�rB	��B	�xB	��B	��B	��B	��B	��B	��B	��B	ĶB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	��B	�B	�B	�B	�B	�<B	�4B	�&B	�$B	�B	�QB	�CB	�OB	�BB	�VB	ބB	ߊB	ߊB	�|B	�B	��B	��B	ܒB	�bB	�nB	�B	�B	�mB	�sB	�B	�B	�B	�B	��B	��B	��B	��B	�B	�B	�B	�B	��B	��B	��B	��B	�3B	��B	��B	��B	�B	�iB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�%B	�B	��B	��B	��B	��B	��B	�6B	�>B	�LB	�tB	�B	�B	�B	�B	�DB	�0B	�B
 B
 B
 4B
 OB	�HB
 4B
'B
;B
'B
AB
AB
;B	�cB
UB
MB
MB
oB
9B
KB
KB
	RB
fB

XB

XB
	lB
^B
~B
jB
jB
VB
VB
\B
pB
�B
�B
�B
�B
}B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
B
�B
�B
�B
 �B
!�B
 �B
�B
�B
!B
"B
#B
#B
"�B
#�B
$B
#�B
$�B
$�B
%B
%B
%�B
%�B
$�B
#�B
$B
$B
%,B
%B
%B
&2B
(
B
(
B
'B
'B
(
B
)B
)B
($B
)*B
*B
+B
+B
+B
*B
+6B
,"B
+B
+B
+B
-B
,"B
,"B
*KB
-)B
-)B
./B
/B
-CB
+QB
-CB
/OB
.IB
/5B
0;B
/OB
-]B
-]B
/5B
0;B
0;B
/OB
0;B
.cB
1AB
33B
2GB
2GB
2GB
3MB
2aB
3MB
2aB
2|B
3hB
2|B
3hB
5ZB
6`B
8RB
8lB
8�B
9XB
:^B
:^B
;dB
8�B
6�B
7�B
9rB
9�B
:xB
<jB
<�B
=qB
<�B
<�B
<�B
>�B
=�B
>�B
?}B
>�B
?�B
?�B
?�B
=�B
@�B
@�B
A�B
@�B
?�B
@�B
A�B
B�B
A�B
A�B
>�B
B�B
C�B
C�B
C�B
D�B
E�B
D�B
D�B
C�B
E�B
E�B
F�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
F�B
G�B
F�B
G�B
H�B
H�B
G�B
G�B
H�B
H�B
H�B
H�B
H�B
J�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
L�B
M�B
L�B
K�B
K�B
J�B
L�B
L�B
L�B
NB
N�B
NB
N�B
N�B
M�B
NB
NB
OB
OB
O�B
O�B
Q B
Q B
Q B
O�B
O�B
Q B
Q B
Q�B
QB
Q B
R B
QB
SB
SB
S&B
SB
TB
SB
TB
TB
T�B
U2B
T,B
TB
UB
T,B
VB
W
B
W$B
W$B
W$B
W$B
W$B
W$B
W$B
W?B
W?B
XEB
XEB
XEB
X+B
Y1B
Y1B
Y1B
Z7B
Z7B
YKB
Z7B
Z7B
YKB
[=B
[=B
[=B
\CB
\CB
\CB
]IB
]IB
]IB
]dB
^OB
]IB
^jB
_VB
^OB
^OB
_VB
_pB
_pB
_pB
`\B
_VB
_pB
`\B
`vB
_�B
`vB
`\B
a|B
a|B
cTB
cTB
c�B
cnB
bhB
dZB
dtB
cnB
dZB
dtB
dtB
d�B
dtB
dtB
dtB
cnB
c�B
d�B
ezB
ezB
dtB
b�B
ezB
gmB
g�B
g�B
g�B
hsB
hsB
h�B
hsB
hsB
h�B
h�B
h�B
g�B
h�B
h�B
h�B
h�B
iyB
h�B
g�B
jB
i�B
i�B
g�B
i�B
k�B
k�B
k�B
j�B
j�B
j�B
k�B
j�B
j�B
l�B
l�B
l�B
l�B
m�B
l�B
l�B
l�B
l�B
l�B
k�B
k�B
l�B
m�B
m�B
l�B
k�B
l�B
l�B
n�B
o�B
n�B
o�B
n�B
o�B
o�B
o�B
p�B
q�B
q�B
q�B
p�B
p�B
p�B
q�B
p�B
q�B
r�B
r�B
s�B
s�B
s�B
r�B
r�B
q�B
q�B
r�B
s�B
s�B
s�B
t�B
t�B
t�B
s�B
t�B
u�B
u�B
t�B
u�B
u�B
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
w�B
w�B
w�B
w�B
w�B
w�B
w�B
xB
v�B
w�B
x�B
w�B
w�B
w�B
y�B
y�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.02(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201809270039272018092700392720180927003927201809270200232018092702002320180927020023201809280025312018092800253120180928002531  JA  ARFMdecpA19c                                                                20180923093515  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180923003536  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180923003539  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180923003540  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180923003540  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180923003540  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180923003540  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180923003540  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180923003541  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180923003541                      G�O�G�O�G�O�                JA  ARUP                                                                        20180923005622                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180923153454  CV  JULD            G�O�G�O�F��                JM  ARGQJMQC2.0                                                                 20180923153454  CV  JULD_LOCATION   G�O�G�O�F��                JM  ARGQJMQC2.0                                                                 20180923153454  CV  LATITUDE        G�O�G�O�A���                JM  ARGQJMQC2.0                                                                 20180923153454  CV  LONGITUDE       G�O�G�O��"��                JM  ARCAJMQC2.0                                                                 20180926153927  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180926153927  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180926170023  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180927152531  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116231516                      G�O�G�O�G�O�                