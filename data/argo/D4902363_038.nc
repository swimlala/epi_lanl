CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2016-09-18T00:35:12Z creation;2016-09-18T00:35:14Z conversion to V3.1;2019-12-19T08:30:10Z update;     
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
_FillValue                 �  I0   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p(   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ͬ   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �@   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �P   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �T   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �X   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �\   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �`   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20160918003512  20200115111517  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               &A   JA  I2_0576_038                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @�˶ۗ�1   @�˶�9 @; ѷX��do����1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @   @�33@�  A   AffA@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B��B   B(  B0ffB8  B@  BHffBPffBW��B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C1�fC4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn�fDo  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~fD~�fD  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�<�DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�C31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�R@��\@�\)@�\)A{A?�A_�A�A��
A��
A��
A��
A��
A��
A��
A��
B�B�B�B�B'�B0Q�B7�B?�BHQ�BPQ�BW�B_�Bg�Bo�Bw�B�B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1�HC3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��qC��C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC�
=C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qD ~�D ��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D	~�D	��D
~�D
��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D�RDxRD��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D ~�D ��D!~�D!��D"~�D"��D#~�D#��D$~�D$��D%~�D%��D&~�D&��D'~�D'��D(~�D(��D)~�D)��D*~�D*��D+~�D+��D,~�D,��D-~�D-��D.~�D.��D/~�D/��D0~�D0��D1~�D1��D2~�D2��D3~�D3��D4~�D4��D5~�D5��D6~�D6��D7~�D7��D8~�D8��D9~�D9��D:~�D:��D;~�D;��D<~�D<��D=~�D=��D>~�D>��D?~�D?��D@~�D@��DA~�DA��DB~�DB��DC~�DC��DD~�DD��DE~�DE��DF~�DF��DG~�DG��DH~�DH��DI~�DI��DJ~�DJ��DK~�DK��DL~�DL��DM~�DM��DN~�DN��DO~�DO��DP~�DP��DQ~�DQ��DR~�DR��DS~�DS��DT~�DT��DU~�DU��DV~�DV��DW~�DW��DX~�DX��DY~�DY��DZ~�DZ��D[~�D[��D\~�D\��D]~�D]��D^~�D^��D_~�D_��D`~�D`��Da~�Da��Db~�Db��Dc~�Dc��Dd~�Dd��De~�De��Df~�Df��Dg~�Dg��Dh~�Dh��Di~�Di��Dj~�Dj��Dk~�Dk��Dl~�Dl��Dm~�Dm��Dn�Dn��Do~�Do��Dp~�Dp��Dq~�Dq��Dr~�Dr��Ds~�Ds��Dt~�Dt��Du~�Du��Dv~�Dv��Dw~�Dw��Dx~�Dx��Dy~�Dy��Dz~�Dz��D{~�D{��D|~�D|��D}~�D~D~�D~��D~�D��D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��)D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D¿\D��\D�?\D�\Dÿ\D��\D�?\D�\DĿ\D��\D�?\D�\Dſ\D��\D�?\D�\Dƿ\D��\D�?\D�\Dǿ\D��\D�?\D�\Dȿ\D��\D�?\D�\Dɿ\D��\D�?\D�\Dʿ\D��\D�?\D�\D˿\D��\D�?\D�\D̿\D��\D�?\D�\DͿ\D��\D�?\D�\Dο\D��\D�?\D�\DϿ\D��\D�?\D�\Dп\D��\D�?\D�\Dѿ\D��\D�?\D�\Dҿ\D��\D�?\D�\Dӿ\D��\D�?\D�\DԿ\D��\D�<)D�\Dտ\D��\D�?\D�\Dֿ\D��\D�?\D�\D׿\D��\D�?\D�\Dؿ\D��\D�?\D�\Dٿ\D��\D�?\D�\Dڿ\D��\D�?\D�\Dۿ\D��\D�?\D�\Dܿ\D��\D�?\D�\Dݿ\D��\D�?\D�\D޿\D��\D�?\D�\D߿\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D��\D��\D�?\D�\D�\D��\D�?\D�\D�)D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��D�B�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��A��A��A��A��A��A��A��A�oA���A��A�M�A�  Aϧ�A�+A�K�A�%A�  A�O�A�33A���A���A�\)Aĕ�A�M�A�G�A���A�%A�/A�{A��A�1A��HA�ffA��;A���A�1A���A�^5A���A�^5A���A��\A���A���A�jA���A�1A�M�A��A��!A���A�=qA�bA�jA��9A��mA�ȴA��uA�VA�C�A�;dA���A��/A�XA���A��A�O�A���A��!A�7LA�`BA�  A�A�ƨA��A�z�A��A���A~�DA|�A{��A{O�A{�AzĜAz�Ay/Ax��Aw;dAv��AuVAt-ArȴAqG�ApZAo;dAm;dAk��Ai�mAh��Af �Ac��Ab�9Ab9XAa�7A`=qA_/A\�+A[�AZ��AYAY/AX��AXn�AW?}AU
=AT9XAS%AR1AQ;dAP�AP��APv�AO\)AN  AM7LAM�AL��ALI�AK�hAJZAI�hAI"�AG�;AGhsAG33AF��AF^5AFAE\)AEVAD�AC�mABJAA��A@�A?x�A>�A>��A>JA=�PA<��A;�;A:�/A:ZA9�A9t�A9%A85?A7�;A7�A7oA6r�A5��A4z�A2�A2�A1�mA1�A0��A0A�A/�#A/K�A.1A-�A,I�A+dZA*��A*JA)��A(��A(E�A'�A&�/A&=qA%�#A%&�A$�9A#��A#t�A#C�A#?}A#&�A#
=A"�A"�DA"=qA!��A!O�A v�AVA$�AdZAoA��A$�A/A�wA\)A�A�`A��AVAbA?}A��AE�A�-A�`A�A��A|�A�A�!AffA��A�A5?A��A��AVA�A+A
��A	t�AM�A`BAVA�A�jA�uAjAE�A|�AĜA��A��AbA I�@���@�E�@�9X@���@�Ĝ@���@�@�@��^@�@���@���@��@�t�@�bN@�9X@�P@�J@���@��m@���@�(�@�V@� �@ՙ�@��`@�33@��H@ҟ�@�^5@���@с@�%@��@ϥ�@�C�@Ο�@��@��H@�hs@ȴ9@�A�@��
@ǝ�@���@���@ũ�@�p�@�t�@�E�@��@���@��
@�
=@�n�@�@��h@�j@�1@��;@��F@��@�dZ@���@�j@�1@�;d@���@�=q@���@�z�@��@��/@�Ĝ@���@��u@���@��;@�t�@�C�@��@�
=@��@���@���@�  @���@�{@���@�  @�S�@��R@�J@��-@�x�@�V@��D@��F@��H@�Q�@�
=@��^@��@���@�p�@�/@�7L@�X@�&�@���@��@���@��@��@�J@�n�@�"�@��R@�=q@��@� �@�1'@�A�@��F@���@�5?@��@��h@��`@��@�1'@�  @���@��
@���@��P@�|�@��P@��F@��@�33@��!@��@�A�@� �@��F@��@�o@��@�\)@�l�@���@���@���@��@�S�@���@��@�@�p�@��`@���@��@�r�@��@�33@��^@���@�`B@��`@��u@�|�@��R@��@�x�@�7L@�V@��j@�I�@���@�ƨ@�ƨ@��w@��F@��@�K�@�C�@�;d@�33@��H@��\@�V@��@�x�@�X@�O�@�/@�V@�%@�V@�/@�?}@�X@��h@��7@��@��@��@��@�hs@�`B@�O�@�?}@���@��@�j@�P@|�@|�@;d@~�+@}��@}`B@|��@|Z@|1@{ƨ@{dZ@{33@z�@z��@zM�@y�#@y��@yx�@yx�@yG�@x�`@x��@x��@y%@x�`@x��@x�u@xA�@w��@w\)@v�y@u�@u/@t�j@t��@t9X@so@r�@r�H@r�!@r=q@q7L@pĜ@pQ�@pb@p  @o�@o�;@o�;@o��@o��@o�w@o��@oK�@o
=@n$�@mp�@m/@l��@l(�@k��@kC�@j�@j�H@j��@j~�@i�#@iG�@hĜ@g�@g\)@g�@f��@f�@f��@fff@f{@f@e�T@e�-@e`B@e�@d�/@d�/@d��@dI�@c�
@c"�@b=q@b�@bJ@a�@a�^@a�^@a��@a�7@aX@a%@`�9@`bN@`A�@`b@_�@_�@_|�@^��@]p�@\�/@\9X@[��@[��@[S�@[C�@[33@[@Z�!@ZJ@YX@Y�@Y%@XĜ@Xr�@W�@W|�@W\)@W;d@W
=@V��@U@U��@U�h@U`B@T��@T��@TZ@T1@Sƨ@S�@S"�@R�!@R~�@Q��@Q�^@Q��@Q�7@Qhs@P�@P �@O�w@O��@OK�@O�@N�y@Nȴ@N�R@N�R@N�+@NE�@N5?@M�@M�h@M`B@M/@MV@L�@L�D@L(�@K��@K�@KS�@K"�@Ko@J�@J�H@J��@J�\@I�#@I�^@I��@I��@Ix�@H��@H1'@G�@G�P@G\)@GK�@G�@Fȴ@E�@E?}@D�@D�D@Dj@DI�@D�@Cƨ@C�@CdZ@C33@B�@B��@Bn�@B=q@A��@AG�@@��@@�@@bN@@1'@@b@@  @?�;@?�@?�P@?l�@?K�@?
=@>�+@>{@=�@=`B@=V@<��@<j@<I�@<9X@<(�@<�@;��@;t�@:�!@:M�@:�@9�@9�#@9�7@97L@8��@8�9@8�9@8�9@8�u@8bN@81'@8 �@8  @7�@7+@6�R@6v�@6V@65?@5�@5@5�@5�@4�j@4z�@49X@3��@3�
@3ƨ@3�F@3��@3��@3t�@3C�@2�@2~�@2=q@1��@1��@1x�@17L@1&�@0�`@0Ĝ@0�9@0bN@0  @/�@/|�@/+@.�y@.ȴ@.�+@.$�@-�-@-�-@-�@-p�@-p�@-?}@,�@,Z@,9X@,9X@+�m@+�@+S�@+"�@+o@*�@*n�@*J@)�@)�^@)��@)��@)7L@(��@(�9@(�@( �@(b@'�@'��@'|�@&��@&ȴ@&�R@&��@&��@&ff@&V@&5?@&{@%�@%��@%`B@%/@$�j@$9X@$�@#ƨ@#��@#dZ@#C�@#33@"�@"��@"�\@"-@!��@!�#@!�^@!�7@!hs@!G�@!&�@ ��@ Ĝ@ �u@ bN@  �@l�@;d@+@
=@�@ff@@�@��@�@`B@?}@/@�@V@��@��@j@�@��@ƨ@��@S�@@~�@^5@=q@-@J@��@��@�@��@��@�7@x�@hs@G�@&�@�@��@�9@��@r�@ �@��@|�@|�@l�@l�@K�@��@��@��@v�@ff@E�@�@@`B@?}@V@��@�/@z�@�
@dZ@33@@�H@�\@~�@=q@�#@�7@G�@�9@�u@r�@�;@�w@�@��@�P@K�@�y@��@E�@�@@�@?}@�@�D@I�@9X@�F@t�@dZ@S�@33@"�@
�@
��@
~�@
n�@
=q@
�@	��@	�^@	x�@	hs@	G�@	7L@	&�@	�@	%@��@�`@�`@�9@1'@�@�@�;@��@|�@|�@l�@l�@\)@\)@\)@\)@+@�@
=@
=@�y@�y@�@�@�@�@�@ȴ@��@V@$�@{@�@��@@�h@O�@�@�j@j@Z@I�@9X@9X@9X@(�@�
@�F@��@�@t�@dZ@33@33@o@��@�\1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��A��A��A��A��A��A��A��A�oA���A��A�M�A�  Aϧ�A�+A�K�A�%A�  A�O�A�33A���A���A�\)Aĕ�A�M�A�G�A���A�%A�/A�{A��A�1A��HA�ffA��;A���A�1A���A�^5A���A�^5A���A��\A���A���A�jA���A�1A�M�A��A��!A���A�=qA�bA�jA��9A��mA�ȴA��uA�VA�C�A�;dA���A��/A�XA���A��A�O�A���A��!A�7LA�`BA�  A�A�ƨA��A�z�A��A���A~�DA|�A{��A{O�A{�AzĜAz�Ay/Ax��Aw;dAv��AuVAt-ArȴAqG�ApZAo;dAm;dAk��Ai�mAh��Af �Ac��Ab�9Ab9XAa�7A`=qA_/A\�+A[�AZ��AYAY/AX��AXn�AW?}AU
=AT9XAS%AR1AQ;dAP�AP��APv�AO\)AN  AM7LAM�AL��ALI�AK�hAJZAI�hAI"�AG�;AGhsAG33AF��AF^5AFAE\)AEVAD�AC�mABJAA��A@�A?x�A>�A>��A>JA=�PA<��A;�;A:�/A:ZA9�A9t�A9%A85?A7�;A7�A7oA6r�A5��A4z�A2�A2�A1�mA1�A0��A0A�A/�#A/K�A.1A-�A,I�A+dZA*��A*JA)��A(��A(E�A'�A&�/A&=qA%�#A%&�A$�9A#��A#t�A#C�A#?}A#&�A#
=A"�A"�DA"=qA!��A!O�A v�AVA$�AdZAoA��A$�A/A�wA\)A�A�`A��AVAbA?}A��AE�A�-A�`A�A��A|�A�A�!AffA��A�A5?A��A��AVA�A+A
��A	t�AM�A`BAVA�A�jA�uAjAE�A|�AĜA��A��AbA I�@���@�E�@�9X@���@�Ĝ@���@�@�@��^@�@���@���@��@�t�@�bN@�9X@�P@�J@���@��m@���@�(�@�V@� �@ՙ�@��`@�33@��H@ҟ�@�^5@���@с@�%@��@ϥ�@�C�@Ο�@��@��H@�hs@ȴ9@�A�@��
@ǝ�@���@���@ũ�@�p�@�t�@�E�@��@���@��
@�
=@�n�@�@��h@�j@�1@��;@��F@��@�dZ@���@�j@�1@�;d@���@�=q@���@�z�@��@��/@�Ĝ@���@��u@���@��;@�t�@�C�@��@�
=@��@���@���@�  @���@�{@���@�  @�S�@��R@�J@��-@�x�@�V@��D@��F@��H@�Q�@�
=@��^@��@���@�p�@�/@�7L@�X@�&�@���@��@���@��@��@�J@�n�@�"�@��R@�=q@��@� �@�1'@�A�@��F@���@�5?@��@��h@��`@��@�1'@�  @���@��
@���@��P@�|�@��P@��F@��@�33@��!@��@�A�@� �@��F@��@�o@��@�\)@�l�@���@���@���@��@�S�@���@��@�@�p�@��`@���@��@�r�@��@�33@��^@���@�`B@��`@��u@�|�@��R@��@�x�@�7L@�V@��j@�I�@���@�ƨ@�ƨ@��w@��F@��@�K�@�C�@�;d@�33@��H@��\@�V@��@�x�@�X@�O�@�/@�V@�%@�V@�/@�?}@�X@��h@��7@��@��@��@��@�hs@�`B@�O�@�?}@���@��@�j@�P@|�@|�@;d@~�+@}��@}`B@|��@|Z@|1@{ƨ@{dZ@{33@z�@z��@zM�@y�#@y��@yx�@yx�@yG�@x�`@x��@x��@y%@x�`@x��@x�u@xA�@w��@w\)@v�y@u�@u/@t�j@t��@t9X@so@r�@r�H@r�!@r=q@q7L@pĜ@pQ�@pb@p  @o�@o�;@o�;@o��@o��@o�w@o��@oK�@o
=@n$�@mp�@m/@l��@l(�@k��@kC�@j�@j�H@j��@j~�@i�#@iG�@hĜ@g�@g\)@g�@f��@f�@f��@fff@f{@f@e�T@e�-@e`B@e�@d�/@d�/@d��@dI�@c�
@c"�@b=q@b�@bJ@a�@a�^@a�^@a��@a�7@aX@a%@`�9@`bN@`A�@`b@_�@_�@_|�@^��@]p�@\�/@\9X@[��@[��@[S�@[C�@[33@[@Z�!@ZJ@YX@Y�@Y%@XĜ@Xr�@W�@W|�@W\)@W;d@W
=@V��@U@U��@U�h@U`B@T��@T��@TZ@T1@Sƨ@S�@S"�@R�!@R~�@Q��@Q�^@Q��@Q�7@Qhs@P�@P �@O�w@O��@OK�@O�@N�y@Nȴ@N�R@N�R@N�+@NE�@N5?@M�@M�h@M`B@M/@MV@L�@L�D@L(�@K��@K�@KS�@K"�@Ko@J�@J�H@J��@J�\@I�#@I�^@I��@I��@Ix�@H��@H1'@G�@G�P@G\)@GK�@G�@Fȴ@E�@E?}@D�@D�D@Dj@DI�@D�@Cƨ@C�@CdZ@C33@B�@B��@Bn�@B=q@A��@AG�@@��@@�@@bN@@1'@@b@@  @?�;@?�@?�P@?l�@?K�@?
=@>�+@>{@=�@=`B@=V@<��@<j@<I�@<9X@<(�@<�@;��@;t�@:�!@:M�@:�@9�@9�#@9�7@97L@8��@8�9@8�9@8�9@8�u@8bN@81'@8 �@8  @7�@7+@6�R@6v�@6V@65?@5�@5@5�@5�@4�j@4z�@49X@3��@3�
@3ƨ@3�F@3��@3��@3t�@3C�@2�@2~�@2=q@1��@1��@1x�@17L@1&�@0�`@0Ĝ@0�9@0bN@0  @/�@/|�@/+@.�y@.ȴ@.�+@.$�@-�-@-�-@-�@-p�@-p�@-?}@,�@,Z@,9X@,9X@+�m@+�@+S�@+"�@+o@*�@*n�@*J@)�@)�^@)��@)��@)7L@(��@(�9@(�@( �@(b@'�@'��@'|�@&��@&ȴ@&�R@&��@&��@&ff@&V@&5?@&{@%�@%��@%`B@%/@$�j@$9X@$�@#ƨ@#��@#dZ@#C�@#33@"�@"��@"�\@"-@!��@!�#@!�^@!�7@!hs@!G�@!&�@ ��@ Ĝ@ �u@ bN@  �@l�@;d@+@
=@�@ff@@�@��@�@`B@?}@/@�@V@��@��@j@�@��@ƨ@��@S�@@~�@^5@=q@-@J@��@��@�@��@��@�7@x�@hs@G�@&�@�@��@�9@��@r�@ �@��@|�@|�@l�@l�@K�@��@��@��@v�@ff@E�@�@@`B@?}@V@��@�/@z�@�
@dZ@33@@�H@�\@~�@=q@�#@�7@G�@�9@�u@r�@�;@�w@�@��@�P@K�@�y@��@E�@�@@�@?}@�@�D@I�@9X@�F@t�@dZ@S�@33@"�@
�@
��@
~�@
n�@
=q@
�@	��@	�^@	x�@	hs@	G�@	7L@	&�@	�@	%@��@�`@�`@�9@1'@�@�@�;@��@|�@|�@l�@l�@\)@\)@\)@\)@+@�@
=@
=@�y@�y@�@�@�@�@�@ȴ@��@V@$�@{@�@��@@�h@O�@�@�j@j@Z@I�@9X@9X@9X@(�@�
@�F@��@�@t�@dZ@33@33@o@��@�\1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�7B�7B�7B�1B�7B�7B�7B�7B�1B�+B�B~�Br�Bs�BffBW
BP�BN�BK�BF�BI�BF�BD�BB�BA�B=qB/BPB��B�fB�jB�B��B��B�oB�VB�DBy�Bt�Bp�B`BBT�BK�B9XB49B.B&�B�B�BhBPBJB1B��B�`B�#BÖB��B�bB�%B~�Bm�BgmBZBN�BE�B8RB+B�BB
��B
�B
�B
�HB
��B
B
�?B
�B
��B
�PB
y�B
r�B
m�B
k�B
gmB
cTB
XB
VB
G�B
C�B
6FB
/B
$�B
�B
hB

=B	��B	�B	�HB	�
B	ƨB	�9B	�B	��B	��B	��B	�uB	�B	u�B	r�B	m�B	hsB	gmB	cTB	^5B	R�B	M�B	I�B	@�B	<jB	8RB	8RB	6FB	1'B	+B	%�B	#�B	#�B	#�B	�B	�B	�B	{B	PB		7B		7B	%B	B	  B��B��B��B��B�B�B�yB�ZB�NB�HB�;B�/B�B�B��B��B��B��B��BɺBǮBƨBŢBĜBB�}B�RB�RB�LB�LB�XB�qB�jB�RB�!B�B��B��B��B��B��B��B��B��B�{B�oB�hB�VB�PB�VB�VB�oB��B��B��B��B��B��B��B�{B�uB�VB�=B�+B�B�B}�By�Bs�Br�Br�Bq�Bq�Bp�Bp�Br�Bq�Bp�Bo�Bm�BjBhsBgmBgmBe`BdZBcTB^5B[#BZBXBVBVBN�BL�BJ�BF�BD�BB�BB�BA�BA�B@�B?}B?}B<jB9XB8RB6FB1'B/B-B)�B'�B�B�B�B�B�B{B{BuB{B{BhBbBbB\BPBPBJB
=BJB
=B1B+B%B%B%B%BBB1B
=BDBJBDB
=B\B\B\BbBuB�B�B�B�B�B�B�B�B�B�B �B"�B"�B#�B!�B"�B%�B%�B%�B&�B(�B'�B)�B+B2-B1'B2-B33B1'B/B33B6FB9XBE�BL�BN�BP�BP�BP�BO�BQ�B]/BaHBcTBhsBt�By�B|�B~�B�B�B�B�+B�7B�DB�JB�\B�DB�+B�1B�uB��B��B��B��B��B��B��B��B��B��B��B��B�B�!B�B�B�B�B�B�B�B��B��B��B�B�-B�9B�?B�RB�dB�jB�qB�wB�}B��BBB��B�}B�^B�XB�dB��BĜB��B��B��B�B�)B�)B�)B�5B�BB�HB�NB�`B�mB�sB�B�B��B��B��B��B	  B	B	B	B	B	B		7B	
=B	DB	PB	\B	oB	uB	uB	{B	�B	�B	�B	�B	�B	�B	�B	 �B	"�B	$�B	)�B	+B	+B	.B	1'B	2-B	33B	6FB	9XB	@�B	D�B	D�B	E�B	E�B	G�B	J�B	L�B	M�B	M�B	M�B	P�B	Q�B	S�B	VB	YB	^5B	`BB	e`B	gmB	iyB	k�B	k�B	m�B	o�B	s�B	u�B	v�B	w�B	x�B	z�B	|�B	|�B	|�B	~�B	�B	�B	�B	�%B	�+B	�1B	�1B	�=B	�JB	�VB	�bB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�'B	�'B	�'B	�-B	�-B	�3B	�3B	�9B	�?B	�LB	�^B	�qB	�wB	�}B	��B	ÖB	ŢB	ǮB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�B	�#B	�)B	�)B	�)B	�/B	�5B	�;B	�HB	�TB	�TB	�TB	�ZB	�ZB	�ZB	�ZB	�ZB	�`B	�fB	�mB	�mB	�sB	�sB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
%B
1B
1B
	7B

=B

=B
DB
DB
JB
JB
JB
JB
PB
PB
PB
VB
VB
\B
\B
\B
bB
hB
oB
oB
oB
uB
uB
uB
uB
uB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
 �B
 �B
!�B
!�B
#�B
#�B
#�B
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
'�B
'�B
(�B
)�B
)�B
)�B
)�B
)�B
+B
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
/B
/B
0!B
0!B
0!B
0!B
0!B
1'B
2-B
2-B
2-B
33B
33B
33B
33B
33B
49B
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
9XB
9XB
9XB
9XB
:^B
:^B
:^B
;dB
;dB
<jB
<jB
<jB
=qB
=qB
=qB
=qB
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
A�B
A�B
B�B
B�B
B�B
B�B
C�B
C�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
G�B
G�B
H�B
H�B
H�B
H�B
H�B
I�B
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
K�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
M�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
O�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
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
XB
XB
XB
XB
XB
YB
YB
YB
ZB
ZB
ZB
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
^5B
^5B
_;B
_;B
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
bNB
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
hsB
hsB
hsB
hsB
hsB
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
k�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
o�B
o�B
p�B
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
p�B
q�B
q�B
q�B
q�B
q�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�7B�7B�7B�1B�7B�7B�RB�lB��B��B��B�uBw�Bw�Bh$BX_BS�BRTBN<BIRBK�BH1BF�BF%BGBJ#B;JB+B[B��B��B�UB��B��B�[B�B��B{JBv�BshBb�BX�BM�B:�B5�B/�B(�B!-BYBB�B�B)B�]B�
B�VB��B�sB�B�1B� Bo BiDB[�BP�BG�B:xB.cB�B�B
��B
�3B
� B
��B
�vB
��B
�zB
��B
��B
�\B
z�B
sMB
m�B
l"B
hXB
dtB
Y1B
W�B
H�B
E9B
7�B
0�B
&�B
�B
&B
~B	��B	��B	�nB	��B	�7B	�ZB	��B	�B	�zB	�qB	�SB	��B	v�B	s�B	nIB	iB	hsB	e,B	`�B	T,B	O\B	J�B	AoB	<�B	8�B	9$B	7�B	2�B	+�B	&2B	$tB	$�B	$�B	!-B	�B	YB	�B	�B		�B		�B	�B	�B	 �B��B��B�B��B�B��B�B�B��B�4B�'B�jB�7B�?BөB҉BѝBϑB͹B�XB�KB�zBƨB��B�MB� B�	B�$B��B�RB�DB�(B�qB��B�'B��B�0B��B��B��B��B��B��B�yB�MB�&B�TB�(B�VB��B��B��B��B��B��B�B�EB�EB�sB��B�2B��B�)B��B��B�BcB{dBtTBsBsBrBrGBqABq�Bs�BrGBq�Bp�Bn�Bk6Bh�Bh$Bh
Be�BeFBd�B_;B\B[#BYeBXBXBO�BNpBL0BG�BEBB�BB�BA�BA�BAB@�B@�B=�B:�B9�B8RB2B0UB.�B,qB*KB 'BBB�BEBMBgB{BBB�B BNB�B�B�B�B�B�B�B�BBYBYBYBtBmB�B�B
rB�B�B0B�B.B�B�B�B�B9B1B�B=B�BxB7BkB�B \B!HB#TB#:B$�B"4B#B&B&2B&�B(sB)yB(XB*�B+�B2�B1�B3hB4�B1�B/OB3MB6zB9�BF?BM6BOBQBQ4BQ4BP}BS[B]�BbBd&Bi_BuZBz^B}qB}B�[B�gB��B��B��B�0B��B�bB�B��B�1B��B��B��B��B�B�HB�4B�B�,B�fB�FB��B��B�qB��B�!B�QB�B�CB��B��B�kB�DB�_B�yB�]B�aB�nB�ZB��B��B��B��B�wB�}B��B��B�B�[B�iB��B��B��B��BāB��B��B��B�B�CB�]B�xB޸B��B�B�B��B�B�B��B�5B�ZB��B�(B�HB	 iB	�B	�B	�B	�B	�B		lB	
�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	B	!B	#:B	%FB	*B	+B	+6B	./B	1'B	2-B	33B	6FB	9XB	@iB	D�B	D�B	E�B	E�B	G�B	J�B	L�B	NB	M�B	N"B	QB	R:B	TaB	VB	Y1B	^jB	`�B	e�B	g�B	i�B	k�B	k�B	m�B	o�B	s�B	u�B	v�B	xB	y	B	z�B	}"B	}B	}B	B	�B	�B	�9B	�?B	�EB	�fB	�fB	�rB	�~B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	� B	�B	�*B	�B	�B	�'B	�AB	�AB	�-B	�-B	�MB	�MB	�nB	�tB	��B	��B	��B	��B	��B	��B	��B	żB	��B	��B	�B	�	B	��B	��B	�"B	�B	�B	�B	�B	�B	�$B	�EB	�+B	�1B	�1B	�QB	�WB	�CB	�CB	�CB	�dB	ބB	ߊB	�B	�nB	�nB	�nB	�tB	�ZB	�tB	�tB	�tB	�zB	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�	B	�	B	�B	��B	��B	�B	�B	�<B	�B	�B	�B	�.B
 B
 B
;B
 B
'B
AB
GB
-B
MB
9B
9B
9B
9B
tB
KB
KB
	RB

XB

XB
^B
^B
JB
JB
dB
~B
jB
jB
�B
pB
pB
vB
�B
�B
�B
�B
�B
�B
�B
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
 �B
 �B
!�B
"B
#�B
#�B
$B
$B
#�B
#�B
$�B
$�B
$�B
$�B
$�B
%B
&2B
&B
'B
(
B
(
B
)*B
*B
*B
*B
*B
*B
+6B
,WB
-)B
-)B
-)B
-)B
./B
./B
./B
/5B
/B
/5B
/5B
/5B
0;B
0;B
0;B
0oB
0UB
1vB
2GB
2GB
2GB
3MB
3MB
3hB
3hB
3�B
4TB
5ZB
5tB
5ZB
5ZB
5?B
5ZB
5ZB
6`B
6`B
6zB
7�B
8lB
8lB
8lB
9�B
9�B
9rB
9�B
:xB
:xB
:xB
;B
;B
<�B
<�B
<�B
=�B
=�B
=�B
=�B
>wB
>�B
>�B
>�B
>�B
>�B
?�B
?�B
?�B
?�B
?�B
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
A�B
A�B
B�B
B�B
B�B
B�B
C�B
C�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
G�B
G�B
H�B
H�B
H�B
H�B
H�B
I�B
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
K�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
M�B
N�B
N�B
N�B
O�B
PB
O�B
O�B
O�B
Q B
QB
Q4B
R B
RB
RB
SB
SB
SB
S&B
T,B
TB
S�B
T,B
S�B
S�B
TB
TB
TB
TB
TB
T�B
UB
UB
UB
UB
VB
VB
VB
VB
V9B
W$B
W
B
W
B
W
B
W$B
W$B
W$B
XB
XEB
X+B
X+B
X+B
Y1B
Y1B
Y1B
Z7B
Z7B
Z7B
[WB
[WB
[WB
\CB
\CB
\CB
\CB
]IB
]IB
]~B
^jB
^OB
_�B
_VB
_VB
_pB
`\B
`BB
`BB
`\B
abB
a|B
abB
a|B
bhB
bhB
bhB
cnB
c�B
cnB
cnB
cnB
c�B
dtB
dZB
dZB
d�B
dtB
d�B
ezB
ezB
ezB
ezB
f�B
f�B
f�B
f�B
gmB
g�B
gmB
gmB
g�B
gmB
h�B
hsB
hsB
h�B
h�B
i�B
jB
j�B
j�B
j�B
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
k�B
k�B
k�B
k�B
k�B
k�B
l�B
l�B
k�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
o�B
o�B
p�B
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
p�B
q�B
q�B
q�B
q�B
q�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<K)_<B�8<-��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.02(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201609220035112016092200351120160922003511201806221214062018062212140620180622121406201804050406462018040504064620180405040646  JA  ARFMdecpA19c                                                                20160918093504  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20160918003512  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20160918003512  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20160918003513  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20160918003514  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20160918003514  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20160918003514  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20160918003514  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20160918003514  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20160918003514                      G�O�G�O�G�O�                JA  ARUP                                                                        20160918012108                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20160918153235  CV  JULD            G�O�G�O�F�]�                JM  ARCAJMQC2.0                                                                 20160921153511  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20160921153511  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404190646  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622031406  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115111517                      G�O�G�O�G�O�                