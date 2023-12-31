CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       ~2017-12-09T15:35:12Z creation;2017-12-09T15:35:15Z conversion to V3.1;2019-12-18T07:26:19Z update;2022-11-21T05:31:34Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         BPRIMARY|https://orcid.org/0000-0001-9150-6442|Kanako Sato, JAMSTEC        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7H   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7X   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7\   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    7`   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7p   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  8   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8H   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    8L   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    8P   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     8T   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8t   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8x   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8|   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8�   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8�   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    9   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    9   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    9   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        :   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  :    PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]8   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  a    TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �8   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �8   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �    TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �P   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �x   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �x   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �x   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �x   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �    HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �H   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �X   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �\   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �l   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �p   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �t   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �xArgo profile    3.1 1.2 19500101000000  20171209153512  20221123111507  4902148 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               yA   JA  I1_0397_121                     2C  Dd�NAVIS_A                         0397                            ARGO 011514                     863 @�;�*���1   @�;���J @;�u��"�d�N;�61   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dty�Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݃3D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D���D�� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111��@���@�\)@�\)A�A?�A_�A�A��
A��
A��
A��
A��
A��
A��
A��
B�B�B�B�B'�B/�B7�B?�BG�BO�BW�B_�Bg�Bo�Bw�B�B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qD ~�D ��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D	~�D	��D
~�D
��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D ~�D ��D!~�D!��D"~�D"��D#~�D#��D$~�D$��D%~�D%��D&~�D&��D'~�D'��D(~�D(��D)~�D)��D*~�D*��D+~�D+��D,~�D,��D-~�D-��D.~�D.��D/~�D/��D0~�D0��D1~�D1��D2~�D2��D3~�D3��D4~�D4��D5~�D5��D6~�D6��D7~�D7��D8~�D8��D9~�D9��D:~�D:��D;~�D;��D<~�D<��D=~�D=��D>~�D>��D?~�D?��D@~�D@��DA~�DA��DB~�DB��DC~�DC��DD~�DD��DE~�DE��DF~�DF��DG~�DG��DH~�DH��DI~�DI��DJ~�DJ��DK~�DK��DL~�DL��DM~�DM��DN~�DN��DO~�DO��DP~�DP��DQ~�DQ��DR~�DR��DS~�DS��DT~�DT��DU~�DU��DV~�DV��DW~�DW��DX~�DX��DY~�DY��DZ~�DZ��D[~�D[��D\~�D\��D]~�D]��D^~�D^��D_~�D_��D`~�D`��Da~�Da��Db~�Db��Dc~�Dc��Dd~�Dd��De~�De��Df~�Df��Dg~�Dg��Dh~�Dh��Di~�Di��Dj~�Dj��Dk~�Dk��Dl~�Dl��Dm~�Dm��Dn~�Dn��Do~�Do��Dp~�Dp��Dq~�Dq��Dr~�Dr��Ds~�Ds��DtxRDt��Du~�Du��Dv~�Dv��Dw~�Dw��Dx~�Dx��Dy~�Dy��Dz~�Dz��D{~�D{��D|~�D|��D}~�D}��D~~�D~��D~�D��D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�|)D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�<)D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D¿\D��\D�?\D�\Dÿ\D��\D�?\D�\DĿ\D��\D�?\D�\Dſ\D��\D�?\D�\Dƿ\D��\D�?\D�\Dǿ\D��\D�?\D�\Dȿ\D��\D�?\D�\Dɿ\D��\D�?\D�\Dʿ\D��\D�?\D�\D˿\D��\D�?\D�\D̿\D��\D�?\D�\DͿ\D��\D�?\D�\Dο\D��\D�?\D�\DϿ\D��\D�?\D�\Dп\D��\D�?\D�\Dѿ\D��\D�?\D�\Dҿ\D��\D�?\D�\Dӿ\D��\D�?\D�\DԿ\D��\D�?\D�\Dտ\D��\D�?\D�\Dֿ\D��\D�?\D�\D׿\D��\D�?\D�\Dؿ\D��\D�?\D�\Dٿ\D��\D�?\D�\Dڿ\D��\D�?\D�\Dۿ\D��\D�?\D�\Dܿ\D��\D�?\D݂�Dݿ\D��\D�?\D�\D޿\D��\D�?\D�\D߿\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D��\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�B�D���D��\11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111�A��RA��RA��RA��^A��jA��jA��RA��RA��-A��9A��9A��!A��-A��wA��^A��A��A�t�A�jA�-A��A�
=A�1A��A��HA��HA��;A��
A���A�ĜA��!A���A��A�z�A�n�A�S�A�-A���A��`A��/A�9XA���A�A�A� �A���A��
A��A��A���A�33A�jA�~�A��`A��A�5?A�r�A�=qA��hA���A��mA��;A�|�A�v�A��A���A��FA��7A�I�A���A���A�oA�I�A�;dA�G�A���A��A��A��`A�z�A�r�A�\)A��A���A~M�A|I�A{?}Ax~�Aw�Aw&�Aw
=Av�At9XAr�DAqx�Aq7LAp�`Ap�\Ao��Am�AlȴAk��Ak�Aj�RAjI�Ai%Ae�PAd�9Ad{Ac�wAb�HAa��A`A�A^��A]�hA]S�A\bA[�AZ��AZ��AZ��AZ^5AYS�AY+AX�+AXVAW�7AVjAU&�AR�AQ7LAP�jAPȴAP�+AP  AOS�AN{AMAL5?AK��AK��AK|�AK�AJ�AI��AI�AHv�AG+AF~�AFQ�AF9XAE��AE�hAEp�AD�AC��AC��AC\)AB��AB  AA?}A@�A@M�A>�+A=S�A:�A:  A8�A6��A6E�A61A5��A4r�A2{A0�DA0A�A/�A/O�A.M�A-&�A+�-A*�A(��A(n�A(I�A(5?A'��A'XA'A&�jA&v�A&5?A%��A%?}A%VA$�/A$�uA$jA$�A#��A"�A"��A"ffA!p�A 9XA��Az�A�AĜA��A�\AA�A�-A��A�+Az�A=qA�7A��AQ�A9XA1'A�A{AJA��A�A�!AVA�AK�AVA^5AdZAȴA9XA�PA�+A�Ap�A��AhsA
n�A	�-A��AbA��A�+A\)AbNAG�A �@��@��h@��m@�|�@��@�=q@�1'@��@�l�@��#@�P@�x�@�Z@�"�@��`@���@�ff@�5?@�x�@�ƨ@���@�n�@���@�A�@�1'@���@�  @�l�@և+@Ձ@���@Ӿw@ёh@�b@�M�@�V@��@�n�@ɑh@ȋD@Ǯ@��H@��@�r�@Õ�@�=q@�V@�ƨ@���@��D@��;@��P@�;d@���@�ȴ@���@�n�@��@�@�p�@�7L@��/@�bN@�t�@��\@���@�/@��/@�r�@�  @�|�@�
=@���@��\@�$�@��D@�+@���@��@�G�@��/@���@���@��u@�(�@��;@�ff@�$�@��7@�\)@��H@��+@��@��@�/@���@��;@��H@��T@���@��@�Ĝ@��9@��@�"�@���@���@�^5@�@�G�@�z�@���@��u@��w@�@�n�@���@�G�@�&�@�%@�%@��/@���@��j@��j@��j@��@�z�@�1'@��@�M�@��^@���@�9X@��@��P@�;d@���@��H@�^5@���@��@� �@�@��!@�v�@�^5@�^5@�^5@�M�@��@�J@�@��@��@��T@���@�G�@���@�Ĝ@��@��;@�S�@�"�@�
=@�@��@��H@�=q@�@�X@�&�@��@���@���@��D@��@��m@���@�C�@���@�~�@�ff@�-@�@��^@��7@�`B@��@��`@��`@��/@���@��j@��u@�Z@�1'@��@~@|Z@{��@{�
@{�
@{��@{S�@z�@z�H@z�!@z�!@z~�@zM�@zJ@y%@x�@x  @w�P@w;d@v��@vV@u�T@u��@u`B@u?}@u/@u/@t��@t�/@u�@u�@u/@u�h@u��@v{@v$�@vE�@vff@vv�@vv�@vE�@u@t�D@s"�@r�\@q�@q��@q�7@q7L@q7L@pĜ@pb@p  @o��@ol�@o
=@n�@n�R@n��@n��@nE�@m�@m@m�-@m�-@m`B@k�
@j�@jM�@i�@ihs@h�9@g�;@f�R@f$�@e�h@e/@dI�@c�F@cdZ@c@b�!@bn�@b�@bJ@bJ@a��@a��@a�^@a��@aG�@a�@`�`@`��@`��@`�u@`A�@` �@` �@`1'@` �@`  @_��@_��@_��@_��@_��@_��@_|�@_+@^�y@^�+@]�@]/@[�
@Y7L@X�9@XA�@W�;@W�w@W��@W|�@W\)@W+@W
=@V��@V�y@V�y@V�R@Vv�@VV@V$�@U��@Tz�@R~�@Qhs@QG�@Q7L@P��@P�@PQ�@O�@O|�@O�@NE�@M�h@M�@L�j@Lj@K��@KS�@K33@Ko@K@J��@I��@H�9@H��@H�u@H�u@H�@Hr�@HbN@G�;@G\)@F�@E�@Dj@D9X@D(�@D�@C��@CdZ@C"�@B�H@B��@B�!@B�!@B�!@B�!@B�!@B�!@B��@B��@B��@B��@B��@B��@B��@B��@B��@B��@BM�@A�#@A��@Ax�@AX@AG�@AG�@A7L@A&�@A�@A%@@�@?�;@>�R@>@=��@=�-@=�@=/@<�@<�@<�@<�D@<�D@<z�@<(�@;�
@;dZ@:��@:^5@:J@:J@9��@9��@8��@8b@8  @8  @7�P@7;d@7�@7
=@6��@65?@5�T@5@5��@5O�@4��@4�D@49X@41@3t�@3o@2��@2�\@2^5@2=q@2-@2J@1�@1�@1�#@1�^@1��@1��@1��@1�7@1x�@1x�@1X@1�@0�`@0��@0��@0�9@0r�@0Q�@/�@.5?@-p�@-?}@-�@,�j@+ƨ@+33@*��@*��@*��@*��@*�\@*M�@)�@)��@)��@)x�@)G�@)7L@)%@(�`@(Ĝ@(�9@(��@(�@(Q�@(1'@(b@(  @'�;@'��@'�P@'+@&�@&�@&�@&�@&ȴ@&�R@&��@&ff@&E�@&5?@%�@%��@%��@%@%�-@%�h@%�@%p�@%O�@%?}@$��@$�@#�F@#��@#�@#t�@#S�@#o@"M�@!�@!��@!�^@!�^@!�#@!��@!��@!�#@!��@!��@!x�@!�@ Ĝ@��@��@�R@��@ff@��@�@�@�/@(�@I�@o@��@��@��@�!@��@�\@�\@~�@~�@n�@n�@n�@n�@^5@^5@^5@-@�@�@�@�@J@��@��@��@�7@hs@�@Ĝ@��@�@ �@�P@K�@�@�y@�R@�+@v�@$�@/@��@�@z�@Z@(�@(�@(�@(�@�m@ƨ@��@�@dZ@dZ@dZ@S�@C�@C�@"�@@�@�H@��@�!@~�@^5@M�@=q@=q@-@-@�@�@�@�@�^@�@�@\)@
=@�y@ȴ@��@V@$�@�T@@@�-@��@��@p�@p�@`B@/@�@�@�@Z@(�@�F@dZ@
�H@
~�@
=q@	��@	��@	�7@	hs@	X@	hs@	hs@	hs@	hs@	X@	X@	G�@	&�@��@�@1'@�@��@��@|�@;d@�@�R@ff@V@@��@`B@/@V@�@��@�D@j@9X@1@�m@�
@ƨ@��@�@t�@dZ@S�@C�@33@"�@o@o@"�@"�@"�@"�@"�@"�@"�@@�H@��@�!@��@~�@n�@n�@^5@^5@^5@M�@M�@M�@=q@-@�#@�^@��@�7@hs@7L@ ��@ ��@ �u@ bN@ A�@ 1'@  �11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111�A��RA��RA��RA��^A��jA��jA��RA��RA��-A��9A��9A��!A��-A��wA��^A��A��A�t�A�jA�-A��A�
=A�1A��A��HA��HA��;A��
A���A�ĜA��!A���A��A�z�A�n�A�S�A�-A���A��`A��/A�9XA���A�A�A� �A���A��
A��A��A���A�33A�jA�~�A��`A��A�5?A�r�A�=qA��hA���A��mA��;A�|�A�v�A��A���A��FA��7A�I�A���A���A�oA�I�A�;dA�G�A���A��A��A��`A�z�A�r�A�\)A��A���A~M�A|I�A{?}Ax~�Aw�Aw&�Aw
=Av�At9XAr�DAqx�Aq7LAp�`Ap�\Ao��Am�AlȴAk��Ak�Aj�RAjI�Ai%Ae�PAd�9Ad{Ac�wAb�HAa��A`A�A^��A]�hA]S�A\bA[�AZ��AZ��AZ��AZ^5AYS�AY+AX�+AXVAW�7AVjAU&�AR�AQ7LAP�jAPȴAP�+AP  AOS�AN{AMAL5?AK��AK��AK|�AK�AJ�AI��AI�AHv�AG+AF~�AFQ�AF9XAE��AE�hAEp�AD�AC��AC��AC\)AB��AB  AA?}A@�A@M�A>�+A=S�A:�A:  A8�A6��A6E�A61A5��A4r�A2{A0�DA0A�A/�A/O�A.M�A-&�A+�-A*�A(��A(n�A(I�A(5?A'��A'XA'A&�jA&v�A&5?A%��A%?}A%VA$�/A$�uA$jA$�A#��A"�A"��A"ffA!p�A 9XA��Az�A�AĜA��A�\AA�A�-A��A�+Az�A=qA�7A��AQ�A9XA1'A�A{AJA��A�A�!AVA�AK�AVA^5AdZAȴA9XA�PA�+A�Ap�A��AhsA
n�A	�-A��AbA��A�+A\)AbNAG�A �@��@��h@��m@�|�@��@�=q@�1'@��@�l�@��#@�P@�x�@�Z@�"�@��`@���@�ff@�5?@�x�@�ƨ@���@�n�@���@�A�@�1'@���@�  @�l�@և+@Ձ@���@Ӿw@ёh@�b@�M�@�V@��@�n�@ɑh@ȋD@Ǯ@��H@��@�r�@Õ�@�=q@�V@�ƨ@���@��D@��;@��P@�;d@���@�ȴ@���@�n�@��@�@�p�@�7L@��/@�bN@�t�@��\@���@�/@��/@�r�@�  @�|�@�
=@���@��\@�$�@��D@�+@���@��@�G�@��/@���@���@��u@�(�@��;@�ff@�$�@��7@�\)@��H@��+@��@��@�/@���@��;@��H@��T@���@��@�Ĝ@��9@��@�"�@���@���@�^5@�@�G�@�z�@���@��u@��w@�@�n�@���@�G�@�&�@�%@�%@��/@���@��j@��j@��j@��@�z�@�1'@��@�M�@��^@���@�9X@��@��P@�;d@���@��H@�^5@���@��@� �@�@��!@�v�@�^5@�^5@�^5@�M�@��@�J@�@��@��@��T@���@�G�@���@�Ĝ@��@��;@�S�@�"�@�
=@�@��@��H@�=q@�@�X@�&�@��@���@���@��D@��@��m@���@�C�@���@�~�@�ff@�-@�@��^@��7@�`B@��@��`@��`@��/@���@��j@��u@�Z@�1'@��@~@|Z@{��@{�
@{�
@{��@{S�@z�@z�H@z�!@z�!@z~�@zM�@zJ@y%@x�@x  @w�P@w;d@v��@vV@u�T@u��@u`B@u?}@u/@u/@t��@t�/@u�@u�@u/@u�h@u��@v{@v$�@vE�@vff@vv�@vv�@vE�@u@t�D@s"�@r�\@q�@q��@q�7@q7L@q7L@pĜ@pb@p  @o��@ol�@o
=@n�@n�R@n��@n��@nE�@m�@m@m�-@m�-@m`B@k�
@j�@jM�@i�@ihs@h�9@g�;@f�R@f$�@e�h@e/@dI�@c�F@cdZ@c@b�!@bn�@b�@bJ@bJ@a��@a��@a�^@a��@aG�@a�@`�`@`��@`��@`�u@`A�@` �@` �@`1'@` �@`  @_��@_��@_��@_��@_��@_��@_|�@_+@^�y@^�+@]�@]/@[�
@Y7L@X�9@XA�@W�;@W�w@W��@W|�@W\)@W+@W
=@V��@V�y@V�y@V�R@Vv�@VV@V$�@U��@Tz�@R~�@Qhs@QG�@Q7L@P��@P�@PQ�@O�@O|�@O�@NE�@M�h@M�@L�j@Lj@K��@KS�@K33@Ko@K@J��@I��@H�9@H��@H�u@H�u@H�@Hr�@HbN@G�;@G\)@F�@E�@Dj@D9X@D(�@D�@C��@CdZ@C"�@B�H@B��@B�!@B�!@B�!@B�!@B�!@B�!@B��@B��@B��@B��@B��@B��@B��@B��@B��@B��@BM�@A�#@A��@Ax�@AX@AG�@AG�@A7L@A&�@A�@A%@@�@?�;@>�R@>@=��@=�-@=�@=/@<�@<�@<�@<�D@<�D@<z�@<(�@;�
@;dZ@:��@:^5@:J@:J@9��@9��@8��@8b@8  @8  @7�P@7;d@7�@7
=@6��@65?@5�T@5@5��@5O�@4��@4�D@49X@41@3t�@3o@2��@2�\@2^5@2=q@2-@2J@1�@1�@1�#@1�^@1��@1��@1��@1�7@1x�@1x�@1X@1�@0�`@0��@0��@0�9@0r�@0Q�@/�@.5?@-p�@-?}@-�@,�j@+ƨ@+33@*��@*��@*��@*��@*�\@*M�@)�@)��@)��@)x�@)G�@)7L@)%@(�`@(Ĝ@(�9@(��@(�@(Q�@(1'@(b@(  @'�;@'��@'�P@'+@&�@&�@&�@&�@&ȴ@&�R@&��@&ff@&E�@&5?@%�@%��@%��@%@%�-@%�h@%�@%p�@%O�@%?}@$��@$�@#�F@#��@#�@#t�@#S�@#o@"M�@!�@!��@!�^@!�^@!�#@!��@!��@!�#@!��@!��@!x�@!�@ Ĝ@��@��@�R@��@ff@��@�@�@�/@(�@I�@o@��@��@��@�!@��@�\@�\@~�@~�@n�@n�@n�@n�@^5@^5@^5@-@�@�@�@�@J@��@��@��@�7@hs@�@Ĝ@��@�@ �@�P@K�@�@�y@�R@�+@v�@$�@/@��@�@z�@Z@(�@(�@(�@(�@�m@ƨ@��@�@dZ@dZ@dZ@S�@C�@C�@"�@@�@�H@��@�!@~�@^5@M�@=q@=q@-@-@�@�@�@�@�^@�@�@\)@
=@�y@ȴ@��@V@$�@�T@@@�-@��@��@p�@p�@`B@/@�@�@�@Z@(�@�F@dZ@
�H@
~�@
=q@	��@	��@	�7@	hs@	X@	hs@	hs@	hs@	hs@	X@	X@	G�@	&�@��@�@1'@�@��@��@|�@;d@�@�R@ff@V@@��@`B@/@V@�@��@�D@j@9X@1@�m@�
@ƨ@��@�@t�@dZ@S�@C�@33@"�@o@o@"�@"�@"�@"�@"�@"�@"�@@�H@��@�!@��@~�@n�@n�@^5@^5@^5@M�@M�@M�@=q@-@�#@�^@��@�7@hs@7L@ ��@ ��@ �u@ bN@ A�@ 1'@  �11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111�B#�B#�B#�B#�B"�B#�B#�B#�B#�B"�B"�B"�B#�B"�B"�B"�B!�B!�B!�B �B �B!�B!�B!�B �B �B �B �B �B �B �B�B�B�B�B�B�B�B�B�BB�9B�%Bk�B_;BXBK�B;dB1'B$�B��B�B�B�#B�B��BƨB�dB�B��B��B��B~�Bo�Bk�BiyBffBaHBN�B>wB49B�BB
�B
�yB
�NB
�B
ȴB
B
��B
�}B
�XB
��B
�oB
�B
|�B
l�B
ffB
cTB
aHB
[#B
O�B
E�B
>wB
<jB
9XB
6FB
1'B
#�B
�B
hB
PB

=B
%B	��B	�TB	�)B	�B	��B	��B	ÖB	�LB	�B	�B	�-B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�hB	�7B	z�B	ffB	_;B	aHB	dZB	cTB	aHB	^5B	XB	YB	T�B	S�B	R�B	P�B	N�B	K�B	I�B	G�B	D�B	?}B	<jB	;dB	:^B	8RB	7LB	6FB	2-B	.B	-B	,B	(�B	$�B	!�B	�B	�B	uB	JB	B��B��B�B�B�B�yB�TB�B��B��B��B��BɺBĜB�}B�XB�FB�9B�9B�3B�'B�!B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�hB�PB�JB�JB�DB�7B�1B�%B�B�B�B�B~�B}�B}�B|�B|�B|�B|�B{�By�Bw�Bv�Bu�Bs�Bq�Bp�Bm�Bk�BiyBgmBe`BbNB_;B\)BXBVBS�BQ�BM�BJ�BH�BF�BC�BB�B@�B?}B>wB=qB=qB<jB:^B8RB6FB49B33B2-B2-B2-B1'B0!B1'B0!B0!B/B/B/B.B-B+B,B.B/B/B/B/B/B.B/B/B1'B2-B2-B49B49B5?B5?B6FB6FB9XB:^B<jB>wB?}BB�BD�BE�BE�BE�BF�BF�BF�BF�BF�BG�BG�BH�BH�BH�BJ�BL�BN�BO�BP�BP�BQ�BR�BQ�B\)B_;B_;BbNBe`BffBhsBiyBk�Bk�Bk�Bk�Bp�Bs�B|�B|�B{�B{�B|�B|�B~�B�B�B�B�%B�1B�=B�DB�PB�VB�VB�\B�uB�{B��B��B��B��B��B��B��B�B�B�B�B�'B�-B�-B�-B�3B�3B�9B�3B�3B�3B�9B�9B�FB�jB�}BĜBǮBɺB��B��B��B��B��B�B�B�5B�fB�yB�B�B�B�B�B�B�B�B�B�B�B�B�B��B��B��B	  B	1B	DB	PB	\B	\B	\B	oB	{B	�B	�B	�B	�B	!�B	#�B	&�B	'�B	(�B	+B	,B	.B	/B	1'B	1'B	33B	5?B	6FB	7LB	8RB	8RB	9XB	9XB	9XB	:^B	;dB	;dB	>wB	E�B	Q�B	XB	[#B	^5B	`BB	aHB	bNB	cTB	cTB	cTB	cTB	bNB	cTB	e`B	e`B	ffB	gmB	hsB	iyB	jB	k�B	l�B	l�B	l�B	m�B	m�B	m�B	n�B	o�B	o�B	p�B	r�B	s�B	u�B	u�B	v�B	w�B	x�B	|�B	~�B	� B	�B	�=B	�JB	�PB	�PB	�PB	�VB	�VB	�\B	�VB	�VB	�VB	�VB	�VB	�VB	�bB	�hB	�hB	�oB	�oB	�oB	�oB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�'B	�-B	�3B	�9B	�?B	�FB	�?B	�FB	�FB	�FB	�LB	�LB	�RB	�XB	�XB	�XB	�^B	�dB	�dB	�dB	�dB	�dB	�jB	�jB	�qB	�qB	�qB	�qB	�qB	�qB	�wB	�}B	��B	B	ĜB	ɺB	��B	��B	�B	�B	�B	�B	�B	�B	�B	�#B	�#B	�#B	�)B	�)B	�/B	�5B	�5B	�;B	�TB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
%B

=B

=B

=B

=B
DB
JB
JB
JB
JB
JB
JB
PB
PB
JB
JB
PB
JB
PB
JB
JB
JB
JB
JB
JB
JB
PB
VB
VB
VB
\B
\B
\B
\B
\B
\B
\B
bB
oB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
#�B
#�B
#�B
#�B
$�B
%�B
%�B
%�B
&�B
'�B
'�B
(�B
(�B
)�B
)�B
+B
+B
+B
,B
,B
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
-B
-B
-B
.B
.B
.B
.B
.B
.B
/B
1'B
33B
33B
33B
49B
5?B
6FB
7LB
8RB
8RB
8RB
8RB
8RB
9XB
9XB
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
;dB
;dB
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
>wB
>wB
>wB
>wB
?}B
?}B
?}B
?}B
?}B
?}B
?}B
@�B
@�B
@�B
@�B
B�B
B�B
B�B
B�B
B�B
C�B
C�B
D�B
E�B
F�B
F�B
F�B
F�B
E�B
E�B
E�B
F�B
F�B
F�B
G�B
G�B
H�B
I�B
J�B
J�B
J�B
K�B
M�B
M�B
L�B
N�B
M�B
O�B
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
Q�B
Q�B
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
T�B
T�B
VB
VB
VB
VB
W
B
W
B
XB
YB
YB
YB
ZB
ZB
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
]/B
]/B
]/B
]/B
]/B
]/B
^5B
^5B
`BB
aHB
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
dZB
dZB
e`B
e`B
e`B
ffB
ffB
gmB
gmB
hsB
hsB
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
jB
jB
k�B
k�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
n�B
n�B
n�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
w�B
w�B
w�B
w�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111�B#�B#�B#�B#�B"�B#�B#�B#�B#�B"�B"�B"�B#�B"�B#B# B!�B!�B"4B �B �B!�B!�B!�B �B �B �B �B �B �B �B�B�B�BBB)BB�BIB�B�dB��Bn}Ba�B[#BN�B>�B5ZB-�B�HB�sB��B�xB�B�uBȀB��B��B��B��B��B��BpoBk�Bj0Bg�BdtBQ�B@�B8lB�BaB
�tB
�QB
�tB
�EB
ɠB
��B
�'B
��B
�PB
��B
��B
��B
}B
m�B
f�B
c�B
b�B
]IB
Q�B
F�B
>�B
<�B
:B
7�B
3hB
%FB
�B
 B
B
DB
1B
 B	�tB	�B	ּB	�,B	�6B	żB	�	B	��B	��B	��B	��B	�BB	��B	��B	�NB	��B	�B	�]B	�1B	��B	�B	�)B	}�B	g�B	_�B	a|B	d�B	d&B	bhB	_�B	YeB	ZB	U�B	TFB	S[B	Q�B	O�B	L~B	J�B	H�B	FB	@4B	<�B	;�B	:�B	8�B	7�B	7LB	3MB	.cB	-�B	,�B	)�B	%�B	"�B	�B	�B	gB	�B	�B�B�+B�B�B�qB�B�B��BՁBөB��B�HB�xBƎB��B��B��B��B��B��B��B��B��B��B��B��B�B�DB�DB�XB�RB��B��B��B�|B�vB�!B�)B��B�B��B��B��B��B��B�	B�B��B�mB��B�B��B�B~(B~B}"B}B}"B}VB|�Bz�BxlBw�Bv�BtTBr�Bq�Bn�BlqBj�Bh�Bf�Bc�Bb4B^BYBW?BUgBS�BP�BLdBJrBH1BEBC�BAUB@�B?}B>B>B=VB<B:^B7fB5�B4�B3�B3B3MB2�B1[B1vB0�B0�B0;B/�B/�B.�B.�B-�B-�B.�B/�B/�B/�B/�B0!B/�B0UB0oB2-B3B3MB4�B5B5�B5�B7B7fB:*B;B=qB?�B@�BCaBEBE�BFBE�BF�BF�BF�BF�BF�BG�BHBIBI7BI�BKxBMjBO\BP.BQNBQhBR�BS�BS�B\�B_�B`\Bc:Be�Bf�Bh�Bi�Bk�Bk�Bk�Bk�Bq'Bt�B}<B}�B}<B|PB}qB}qBcB�aB��B��B��B��B��B��B��B��B��B�B��B��B��B�B�NB�ZB��B�>B��B��B��B��B�iB�AB�GB�GB�aB�MB�hB�TB�MB�hB��B��B��B�2B��B�4B�B��B�	B�B�B�B�vB҉B֡B��B��B�B��B�B�B�B�B�B��B��B�B�B�B��B�B�B��B�B�xB	 iB	KB	xB	jB	�B	�B	�B	�B	�B	�B	�B	�B	�B	"B	$&B	'B	(>B	)DB	+QB	,qB	./B	/iB	1vB	1vB	3hB	5tB	6zB	7fB	8RB	8lB	9rB	9rB	9�B	:�B	;�B	;�B	?B	F?B	R B	X+B	[WB	^jB	`vB	a|B	bhB	cnB	cnB	cnB	cnB	b�B	c�B	e�B	e�B	f�B	g�B	h�B	i�B	j�B	k�B	l�B	l�B	l�B	m�B	m�B	m�B	n�B	o�B	o�B	p�B	r�B	s�B	u�B	u�B	v�B	w�B	x�B	}<B	HB	��B	��B	�rB	�~B	�jB	�jB	�jB	�pB	��B	��B	�pB	�pB	��B	��B	��B	�pB	�}B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	� B	�2B	�RB	�6B	�=B	�=B	�WB	�iB	�UB	�[B	�GB	�MB	�TB	�ZB	�FB	�ZB	�`B	�zB	�`B	��B	�fB	�lB	�rB	�rB	�rB	�xB	�B	�dB	�B	�B	��B	��B	��B	�qB	�qB	�qB	��B	��B	��B	��B	��B	��B	��B	�9B	ʌB	�&B	�2B	�9B	�+B	�+B	�+B	�1B	�1B	�7B	�#B	�=B	�=B	�CB	�CB	�IB	�jB	ބB	߾B	��B	��B	�B	�B	��B	�B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�B	��B	��B	�B	��B	�0B	�0B	�<B
 B
 B
B
 B
 B
 B
;B
AB
aB
�B
�B

XB

XB

XB

XB
�B
dB
dB
dB
JB
JB
JB
PB
PB
JB
JB
PB
JB
PB
JB
dB
JB
JB
dB
dB
~B
�B
�B
�B
pB
\B
vB
\B
vB
vB
vB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
yB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
!�B
!�B
!�B
"�B
#�B
#�B
$&B
$B
$�B
&B
%�B
&B
'B
(
B
(
B
)B
)*B
*B
*B
+6B
+B
+B
,B
,=B
,"B
,B
,"B
,"B
,B
,B
-)B
-B
-B
-B
-)B
-)B
-CB
.B
./B
./B
./B
./B
.cB
/�B
1vB
3hB
3hB
3hB
4�B
5tB
6`B
7fB
8RB
8lB
8lB
8lB
8lB
9rB
9rB
:xB
:xB
:xB
:�B
;B
;B
;B
;B
;B
;�B
;�B
;B
<�B
<�B
<�B
<�B
<�B
=�B
=VB
=qB
=qB
=qB
=�B
=�B
>�B
>�B
>�B
>�B
?�B
?}B
?�B
?}B
?�B
?�B
?�B
@�B
@�B
@�B
@�B
B�B
B�B
B�B
B�B
B�B
C�B
C�B
D�B
E�B
F�B
F�B
F�B
F�B
E�B
E�B
E�B
F�B
F�B
F�B
G�B
G�B
IB
I�B
J�B
J�B
KB
K�B
M�B
M�B
MB
OB
N<B
O�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
RB
Q�B
Q�B
RB
Q�B
Q�B
RB
Q�B
RB
Q�B
RB
Q�B
Q�B
Q�B
RB
RB
R�B
SB
SB
SB
SB
TB
TB
TFB
T,B
UB
UB
VB
VB
VB
VB
W?B
WYB
X+B
Y1B
Y1B
Y1B
Z7B
ZB
ZB
ZB
Z7B
ZQB
[WB
[=B
[=B
[#B
[#B
[#B
[=B
\)B
\CB
\CB
\)B
\)B
\CB
\]B
]IB
]IB
]/B
]/B
]/B
]/B
]/B
]IB
]/B
]/B
]IB
]dB
^�B
^�B
`vB
abB
bhB
bhB
bhB
bhB
cnB
cnB
cnB
dZB
dZB
dtB
dZB
dtB
dZB
dtB
dtB
dtB
dtB
ezB
ezB
ezB
f�B
f�B
g�B
g�B
h�B
h�B
i�B
i�B
i�B
jB
jB
jB
jB
jB
jB
jB
j�B
j�B
j�B
j�B
k�B
k�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
n�B
n�B
n�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
w�B
w�B
w�B
w�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.02(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201712220035112017122200351120171222003511202211182132512022111821325120221118213251201804031938272018040319382720180403193827  JA  ARFMdecpA19c                                                                20171210003511  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20171209153512  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20171209153514  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20171209153514  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20171209153515  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20171209153515  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20171209153515  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20171209153515  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20171209153515  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20171209153515                      G�O�G�O�G�O�                JA  ARUP                                                                        20171209155514                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20171209153140  CV  JULD            G�O�G�O�F��                JM  ARCAJMQC2.0                                                                 20171221153511  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20171221153511  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180403103827  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200114171536                      G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20221118123251  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20221123111507                      G�O�G�O�G�O�                