CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2019-07-22T15:43:26Z creation;2019-07-22T15:43:33Z conversion to V3.1;2022-11-21T05:28:37Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         BPRIMARY|https://orcid.org/0000-0001-9150-6442|Kanako Sato, JAMSTEC        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7,   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7<   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7@   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    7D   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7T   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7d   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7t   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7|   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8,   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    80   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    84   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     88   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8X   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8\   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8`   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
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
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    9    CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        :    PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  :   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]8   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  a$   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �X   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �D   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �x   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �d   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ά   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �P   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �`   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �d   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �h   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �l   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �p   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20190722154326  20221123111508  4902148 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I1_0397_180                     2C  D   NAVIS_A                         0397                            ARGO 011514                     863 @���-" 1   @�� ��O�@<<"h	ԕ�dg�K]�1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  Aљ�A�33A�33B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT�CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DA��DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ Dü�D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D��3D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dу3D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�C3D�� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @���@�\)@�\)A�A?�A_�A�A��
A��
A��
A��
A�p�A�
=A�
=A��
B�B�B�B�B'�B/�B7�B?�BG�BO�BW�B_�Bg�Bo�Bw�B�B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CT{CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qD ~�D ��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D	~�D	��D
~�D
��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�DD~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D ~�D ��D!~�D!��D"~�D"��D#~�D#��D$~�D$��D%~�D%��D&~�D&��D'~�D'��D(~�D(��D)~�D)��D*~�D*��D+~�D+��D,~�D,��D-~�D-��D.~�D.��D/~�D/��D0~�D0��D1~�D1��D2~�D2��D3~�D3��D4~�D4��D5~�D5��D6~�D6��D7~�D7��D8~�D8��D9~�D9��D:~�D:��D;~�D;��D<~�D<��D=~�D=��D>~�D>��D?~�D?��D@~�D@��DA~�DA�RDB~�DB��DC~�DC��DD~�DD��DE~�DE��DF~�DF��DG~�DG��DH~�DH��DI~�DI��DJ~�DJ��DK~�DK��DL~�DL��DM~�DM��DN~�DN��DO~�DO��DP~�DP��DQ~�DQ��DR~�DR��DS~�DS��DT~�DT��DU~�DU��DV~�DV��DW~�DW��DX~�DX��DY~�DY��DZ~�DZ��D[~�D[��D\~�D\��D]~�D]��D^~�D^��D_~�D_��D`~�D`��Da~�Da��Db~�Db��Dc~�Dc��Dd~�Dd��De~�De��Df~�Df��Dg~�Dg��Dh~�Dh��Di~�Di��Dj~�Dj��Dk~�Dk��Dl~�Dl��Dm~�Dm��Dn~�Dn��Do~�Do��Dp~�Dp��Dq~�Dq��Dr~�Dr��Ds~�Ds��Dt~�Dt��Du~�Du��Dv~�Dv��Dw~�Dw��Dx~�Dx��Dy~�Dy��Dz~�Dz��D{~�D{��D|~�D|��D}~�D}��D~~�D~��D~�D��D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��D�B�D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��)D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D¿\D��\D�?\D�\Dü)D��\D�?\D�\DĿ\D��\D�?\D�\Dſ\D��\D�?\D�\Dƿ\D��\D�?\D�\Dǿ\D��\D�?\D�\Dȿ\D��\D�?\D�\Dɿ\D��\D�?\D�\D�D��\D�?\D�\D˿\D��\D�?\D�\D̿\D��\D�?\D�\DͿ\D��\D�?\D�\Dο\D��\D�?\D�\DϿ\D��\D�?\D�\Dп\D��\D�?\Dт�Dѿ\D��\D�?\D�\Dҿ\D��\D�?\D�\Dӿ\D��\D�?\D�\DԿ\D��\D�?\D�\Dտ\D��\D�?\D�\Dֿ\D��\D�?\D�\D׿\D��\D�?\D�\Dؿ\D��\D�?\D�\Dٿ\D��\D�?\D�\Dڿ\D��\D�?\D�\Dۿ\D��\D�?\D�\Dܿ\D��\D�?\D�\Dݿ\D��\D�?\D�\D޿\D��\D�?\D�\D߿\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D��\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D���D��\D��\D�B�D�\11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   Aԉ7AԅAԉ7AԍPAԓuAԙ�Aԙ�Aԕ�Aԕ�Aԗ�Aԇ+Aӟ�A�$�A�
=A���A�"�A���A��HA�(�A��A�33A���A���A��A���A���A�l�A��#A�C�A��A�bA��;A�E�A�K�A��hA��A���A�`BA�ĜA��TA�1'A��uA��;A���A��RA���A��/A�A�?}A�v�A���A� �A���A�K�A���A���A�9XA�hsA�C�A�"�A��^A�|�A�?}A��A��hA�1'A��wA�VA��jA�1A���A���A��hA��HA���A��A�=qA��-A��A���A�v�A��A���A�=qA�33A�VA��
A���A�$�A�%A���A�dZA�$�A���A�;dA�;A~ĜA{��Ay|�Axn�Aw/Au��AsoAqK�Ap�AoS�Am��AlA�AkS�AjJAg��Af�\Af �AeS�Ab�`A_��A]A\n�A[dZAZ�RAZffAY��AYXAX�AX^5AW�AVz�AU�AS�#ARn�APbNAOhsAO|�AO�PAO%AO�AN��ANE�AM`BAL=qAK"�AIƨAHffAG�^AFI�AE�#AE�AEVAD�\AD~�AD=qAC�AB�!ABJA@�HA@JA>�`A=\)A;�#A;?}A:�`A9/A7K�A5�A2I�A0��A0v�A0�A/dZA.�A,�!A+O�A*ffA* �A*�A*bA)��A)ƨA)p�A)XA)?}A)VA(��A(5?A'�hA&�9A%�7A#�
A!p�A JAl�A��A1A�AS�A��A�+AbAp�A&�A�9A5?Ax�AJA(�A�Ap�A33A��AbNAA�A�PA�DA��A�A�\A��A��AĜAQ�A7LA�DA��A
�A	��A��AVA�;A�HA�A?}A�AE�A�PA �D@��y@�7L@���@�(�@��@�n�@��^@��@���@��P@�=q@��`@�1'@�K�@�5?@�X@�V@��/@��`@���@�9@��y@�%@�(�@�l�@��@�n�@陚@��@��`@��#@�dZ@�+@��@�Z@�\)@�V@�@��@ܣ�@�"�@��T@ٙ�@ف@���@�l�@�@�r�@�K�@�ȴ@�ff@��#@��`@�I�@�1@��@��@Ͼw@�o@�-@���@��m@�S�@�ȴ@�^5@�G�@���@Ƨ�@�O�@�bN@Ý�@�;d@��y@�hs@�A�@�l�@���@�/@��@���@���@�V@��@�5?@�j@���@�@�@��h@�p�@��@��j@�1'@��m@�l�@�"�@��y@�$�@�`B@�j@���@�o@�@���@��P@��@�n�@�`B@��D@�  @���@�{@��@�j@�  @��@�n�@�J@�p�@�/@�%@��j@�r�@�Q�@��@�+@���@�v�@�V@��@��@���@�7L@���@�j@�(�@�  @��;@��w@��P@�33@��y@���@�-@�J@�@��@��@��@���@�`B@�j@�I�@�I�@�(�@��;@�;d@��y@���@���@��!@�M�@��@�@���@��@�9X@�1'@� �@��w@��P@�\)@�K�@�33@�"�@�
=@��R@�V@��#@�&�@���@�b@�  @���@��y@��+@��@��@���@��-@���@��h@�hs@�G�@��`@��D@�1@��P@�33@���@���@��@��R@�E�@���@���@��#@��h@�V@��`@��@��@�Q�@��m@�ƨ@�+@�~�@�^5@��@��^@�x�@��/@���@�Ĝ@��@�bN@�(�@�@�w@;d@
=@~�y@~ȴ@~�R@~��@~v�@~E�@~{@}�T@}�@|�/@|1@{�@z��@z=q@y�@y��@yhs@y%@x��@x��@xr�@xA�@xA�@xA�@x1'@x  @w�w@w
=@vff@vE�@v@tI�@s��@st�@s33@so@r��@r-@q�^@q��@qx�@qX@q�@p��@pĜ@p�@o�;@o\)@o
=@n�@n�R@n@mO�@l��@m�@m?}@mV@l��@kƨ@k�@k�@kC�@i�@h��@h  @f��@f�R@f��@f{@eV@dI�@cƨ@cC�@b��@b~�@bM�@a��@a%@`�@`1'@_�@_|�@_+@^�y@^��@^V@]��@\�/@\�@\I�@[��@[�
@[ƨ@[��@[��@[t�@[dZ@[33@Z��@Z��@Z~�@Z-@ZJ@Y��@Y��@Yhs@X��@XĜ@Xr�@X �@W�@W�w@W|�@W
=@V��@V{@U�T@U�h@Up�@U/@T�/@T�@T�D@Tz�@Tz�@Tz�@T�D@T�D@Tz�@Tj@TZ@TI�@T1@S�
@St�@S@R=q@Q��@Q��@Qx�@QG�@P�u@Pr�@PbN@PQ�@P �@P  @O�;@O\)@N��@Nv�@N5?@M�@Mp�@Lz�@L(�@L�@K��@K�
@K�F@K��@K��@K��@K��@K�@KC�@Ko@J�H@J�!@J~�@JM�@JJ@I�#@Ix�@H�`@H�9@HQ�@G�;@G�w@G�w@GK�@G+@G�@F�@F�R@F�+@E�T@Ep�@E?}@E�@D��@D9X@D�@Cƨ@C��@C33@C"�@C"�@B�H@B��@B~�@BM�@BJ@A��@A��@AX@AG�@A�@@��@@�u@@ �@?�;@?�@?\)@?�@?�@>�y@>�+@>5?@=�@<�j@<I�@<(�@<�@;�
@;�F@;��@;�@;S�@;"�@:�@:�\@9�^@8Ĝ@8Q�@8  @7|�@7K�@7K�@7+@7�@6�@6ff@6@5��@5`B@5�@4�@4��@4Z@4�@3t�@3S�@3C�@3C�@3C�@333@3o@2��@2�!@2n�@2=q@1��@1�7@1hs@1X@1&�@1�@1%@0��@0�u@/�@/�;@/��@/�@/�@/��@/�P@/K�@/;d@/
=@.�y@.��@.�+@.V@.@-@-p�@-�@,�@,z�@,I�@,1@+�m@+�@+"�@*n�@*�@*J@)��@)��@)�@)��@)�^@)hs@(Ĝ@(bN@(Q�@(A�@(1'@'�@'\)@&��@&�R@&�+@&�+@&��@&ff@&5?@&@&@%�@%�T@%��@%�h@%?}@$�/@$�j@$�@$I�@#�m@#S�@#33@#"�@#o@"��@"n�@"=q@"-@"-@"=q@"-@"J@!�@!hs@ ��@ ��@ r�@ bN@ Q�@ b@   @�@�@�;@��@�P@
=@�@�R@�+@ff@5?@5?@{@@�@O�@��@j@I�@1@ƨ@��@��@dZ@"�@�@��@��@~�@M�@�@��@x�@X@&�@�`@bN@  @�@�P@\)@+@
=@��@�@v�@$�@�@�T@@�h@O�@��@��@��@�@�
@S�@�@��@n�@-@��@x�@hs@X@�@��@�`@�u@r�@Q�@A�@1'@  @�;@�w@�w@l�@ȴ@�+@V@5?@{@�@��@�-@�h@p�@?}@�@��@��@�D@j@Z@Z@I�@(�@1@��@ƨ@t�@C�@C�@33@33@"�@@
�H@
��@
~�@
n�@
=q@
�@	�#@	�#@	��@	��@	x�@	hs@	G�@	�@��@b@�;@�w@�@�@�@;d@��@�@��@�+@v�@ff@V@@�@�T@�T@��@��@O�@��@�/@�/@�D@9X@�m@ƨ@��@��@��@�@�@�@t�@t�@dZ@C�@33@��@�\@^5@�@J@�@��@��@�7@x�@X@�@ ��@ ��@ ��@ ��@ �@ r�@ bN@ r�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   Aԉ7AԅAԉ7AԍPAԓuAԙ�Aԙ�Aԕ�Aԕ�Aԗ�Aԇ+Aӟ�A�$�A�
=A���A�"�A���A��HA�(�A��A�33A���A���A��A���A���A�l�A��#A�C�A��A�bA��;A�E�A�K�A��hA��A���A�`BA�ĜA��TA�1'A��uA��;A���A��RA���A��/A�A�?}A�v�A���A� �A���A�K�A���A���A�9XA�hsA�C�A�"�A��^A�|�A�?}A��A��hA�1'A��wA�VA��jA�1A���A���A��hA��HA���A��A�=qA��-A��A���A�v�A��A���A�=qA�33A�VA��
A���A�$�A�%A���A�dZA�$�A���A�;dA�;A~ĜA{��Ay|�Axn�Aw/Au��AsoAqK�Ap�AoS�Am��AlA�AkS�AjJAg��Af�\Af �AeS�Ab�`A_��A]A\n�A[dZAZ�RAZffAY��AYXAX�AX^5AW�AVz�AU�AS�#ARn�APbNAOhsAO|�AO�PAO%AO�AN��ANE�AM`BAL=qAK"�AIƨAHffAG�^AFI�AE�#AE�AEVAD�\AD~�AD=qAC�AB�!ABJA@�HA@JA>�`A=\)A;�#A;?}A:�`A9/A7K�A5�A2I�A0��A0v�A0�A/dZA.�A,�!A+O�A*ffA* �A*�A*bA)��A)ƨA)p�A)XA)?}A)VA(��A(5?A'�hA&�9A%�7A#�
A!p�A JAl�A��A1A�AS�A��A�+AbAp�A&�A�9A5?Ax�AJA(�A�Ap�A33A��AbNAA�A�PA�DA��A�A�\A��A��AĜAQ�A7LA�DA��A
�A	��A��AVA�;A�HA�A?}A�AE�A�PA �D@��y@�7L@���@�(�@��@�n�@��^@��@���@��P@�=q@��`@�1'@�K�@�5?@�X@�V@��/@��`@���@�9@��y@�%@�(�@�l�@��@�n�@陚@��@��`@��#@�dZ@�+@��@�Z@�\)@�V@�@��@ܣ�@�"�@��T@ٙ�@ف@���@�l�@�@�r�@�K�@�ȴ@�ff@��#@��`@�I�@�1@��@��@Ͼw@�o@�-@���@��m@�S�@�ȴ@�^5@�G�@���@Ƨ�@�O�@�bN@Ý�@�;d@��y@�hs@�A�@�l�@���@�/@��@���@���@�V@��@�5?@�j@���@�@�@��h@�p�@��@��j@�1'@��m@�l�@�"�@��y@�$�@�`B@�j@���@�o@�@���@��P@��@�n�@�`B@��D@�  @���@�{@��@�j@�  @��@�n�@�J@�p�@�/@�%@��j@�r�@�Q�@��@�+@���@�v�@�V@��@��@���@�7L@���@�j@�(�@�  @��;@��w@��P@�33@��y@���@�-@�J@�@��@��@��@���@�`B@�j@�I�@�I�@�(�@��;@�;d@��y@���@���@��!@�M�@��@�@���@��@�9X@�1'@� �@��w@��P@�\)@�K�@�33@�"�@�
=@��R@�V@��#@�&�@���@�b@�  @���@��y@��+@��@��@���@��-@���@��h@�hs@�G�@��`@��D@�1@��P@�33@���@���@��@��R@�E�@���@���@��#@��h@�V@��`@��@��@�Q�@��m@�ƨ@�+@�~�@�^5@��@��^@�x�@��/@���@�Ĝ@��@�bN@�(�@�@�w@;d@
=@~�y@~ȴ@~�R@~��@~v�@~E�@~{@}�T@}�@|�/@|1@{�@z��@z=q@y�@y��@yhs@y%@x��@x��@xr�@xA�@xA�@xA�@x1'@x  @w�w@w
=@vff@vE�@v@tI�@s��@st�@s33@so@r��@r-@q�^@q��@qx�@qX@q�@p��@pĜ@p�@o�;@o\)@o
=@n�@n�R@n@mO�@l��@m�@m?}@mV@l��@kƨ@k�@k�@kC�@i�@h��@h  @f��@f�R@f��@f{@eV@dI�@cƨ@cC�@b��@b~�@bM�@a��@a%@`�@`1'@_�@_|�@_+@^�y@^��@^V@]��@\�/@\�@\I�@[��@[�
@[ƨ@[��@[��@[t�@[dZ@[33@Z��@Z��@Z~�@Z-@ZJ@Y��@Y��@Yhs@X��@XĜ@Xr�@X �@W�@W�w@W|�@W
=@V��@V{@U�T@U�h@Up�@U/@T�/@T�@T�D@Tz�@Tz�@Tz�@T�D@T�D@Tz�@Tj@TZ@TI�@T1@S�
@St�@S@R=q@Q��@Q��@Qx�@QG�@P�u@Pr�@PbN@PQ�@P �@P  @O�;@O\)@N��@Nv�@N5?@M�@Mp�@Lz�@L(�@L�@K��@K�
@K�F@K��@K��@K��@K��@K�@KC�@Ko@J�H@J�!@J~�@JM�@JJ@I�#@Ix�@H�`@H�9@HQ�@G�;@G�w@G�w@GK�@G+@G�@F�@F�R@F�+@E�T@Ep�@E?}@E�@D��@D9X@D�@Cƨ@C��@C33@C"�@C"�@B�H@B��@B~�@BM�@BJ@A��@A��@AX@AG�@A�@@��@@�u@@ �@?�;@?�@?\)@?�@?�@>�y@>�+@>5?@=�@<�j@<I�@<(�@<�@;�
@;�F@;��@;�@;S�@;"�@:�@:�\@9�^@8Ĝ@8Q�@8  @7|�@7K�@7K�@7+@7�@6�@6ff@6@5��@5`B@5�@4�@4��@4Z@4�@3t�@3S�@3C�@3C�@3C�@333@3o@2��@2�!@2n�@2=q@1��@1�7@1hs@1X@1&�@1�@1%@0��@0�u@/�@/�;@/��@/�@/�@/��@/�P@/K�@/;d@/
=@.�y@.��@.�+@.V@.@-@-p�@-�@,�@,z�@,I�@,1@+�m@+�@+"�@*n�@*�@*J@)��@)��@)�@)��@)�^@)hs@(Ĝ@(bN@(Q�@(A�@(1'@'�@'\)@&��@&�R@&�+@&�+@&��@&ff@&5?@&@&@%�@%�T@%��@%�h@%?}@$�/@$�j@$�@$I�@#�m@#S�@#33@#"�@#o@"��@"n�@"=q@"-@"-@"=q@"-@"J@!�@!hs@ ��@ ��@ r�@ bN@ Q�@ b@   @�@�@�;@��@�P@
=@�@�R@�+@ff@5?@5?@{@@�@O�@��@j@I�@1@ƨ@��@��@dZ@"�@�@��@��@~�@M�@�@��@x�@X@&�@�`@bN@  @�@�P@\)@+@
=@��@�@v�@$�@�@�T@@�h@O�@��@��@��@�@�
@S�@�@��@n�@-@��@x�@hs@X@�@��@�`@�u@r�@Q�@A�@1'@  @�;@�w@�w@l�@ȴ@�+@V@5?@{@�@��@�-@�h@p�@?}@�@��@��@�D@j@Z@Z@I�@(�@1@��@ƨ@t�@C�@C�@33@33@"�@@
�H@
��@
~�@
n�@
=q@
�@	�#@	�#@	��@	��@	x�@	hs@	G�@	�@��@b@�;@�w@�@�@�@;d@��@�@��@�+@v�@ff@V@@�@�T@�T@��@��@O�@��@�/@�/@�D@9X@�m@ƨ@��@��@��@�@�@�@t�@t�@dZ@C�@33@��@�\@^5@�@J@�@��@��@�7@x�@X@�@ ��@ ��@ ��@ ��@ �@ r�@ bN@ r�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�B�B�B�B�B�B�B�B�B�B�B� Bu�B}�BǮB�
B�#B�B�#B�B�B�B��B�B�#B�#B��B��B��B��BȴBǮBŢB��B�}B�}B�qB�^B�?B�B��B��B��B��B�\B�Bz�Bq�BjBbNBZBR�BN�BH�BB�B=qB7LB,B(�B&�B!�B �B �B�B�B1BB�B�)B��BƨB�XB��B��B�uB�7B{�Br�BhsBbNBZBJ�B7LB�BPB
��B
��B
�B
�sB
�B
��B
��B
�dB
��B
�PB
�+B
|�B
gmB
R�B
L�B
E�B
9XB
$�B
�B
bB
+B	��B	��B	�B	�ZB	��B	��B	ƨB	��B	�B	��B	�PB	�B	~�B	y�B	w�B	s�B	o�B	l�B	iyB	aHB	]/B	VB	J�B	C�B	9XB	49B	5?B	6FB	7LB	=qB	<jB	=qB	;dB	5?B	/B	%�B	�B	�B	�B	uB	hB	\B	JB	DB		7B	B��B��B��B�B�yB�5B�#B�5B�)B��B��BĜB�^B�9B�3B�'B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�bB�JB�+B�B}�B{�Bx�Bv�Bu�Bt�Bs�Br�Bp�Bn�Bl�Bk�BiyBffBbNB]/B[#B[#BZB[#BZBYBW
BS�BP�BN�BM�BJ�BG�BF�BD�BB�B@�B=qB9XB7LB33B0!B.B-B,B)�B(�B&�B$�B"�B!�B �B �B�B�B�B�B�B�B�B �B"�B"�B"�B#�B#�B#�B#�B#�B#�B"�B#�B#�B!�B�B�B�B�B$�B+B/B0!B0!B1'B1'B2-B33B33B2-B2-B33B33B2-B2-B1'B0!B/B/B0!B1'B1'B0!B0!B0!B0!B0!B0!B0!B0!B/B1'B1'B1'B1'B1'B2-B33B1'B.B-B,B+B+B-B/B0!B1'B0!B0!B/B/B.B.B.B2-B5?B7LB7LB8RB8RB8RB9XB;dB<jB=qB>wB>wB@�BB�BF�BG�BJ�BM�BQ�BW
BYB[#B_;BbNBdZBhsBk�Bp�Bs�Bt�Bx�B{�B}�B�B�B�B�B�%B�%B�7B�DB�VB�\B�bB�hB�oB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�9B�?B�?B�?B�LB�dB�qB�wB�wB�wB��B��B��BÖBȴBɺB��B��B��B��B��B��B��B��B��B�B�B�)B�HB�ZB�sB�sB�B�B�B��B��B��B��B��B��B��B��B��B	B	%B		7B	DB	PB	PB	VB	\B	oB	{B	{B	�B	�B	�B	�B	�B	�B	 �B	#�B	&�B	-B	1'B	1'B	6FB	9XB	;dB	@�B	@�B	A�B	C�B	D�B	F�B	G�B	H�B	J�B	K�B	K�B	L�B	L�B	M�B	M�B	N�B	N�B	N�B	P�B	S�B	VB	XB	\)B	`BB	bNB	bNB	bNB	dZB	e`B	ffB	gmB	iyB	iyB	iyB	iyB	k�B	l�B	o�B	q�B	q�B	r�B	y�B	z�B	}�B	~�B	� B	� B	�B	�B	�B	�B	�%B	�%B	�+B	�+B	�1B	�=B	�JB	�VB	�VB	�VB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�3B	�FB	�XB	�^B	�qB	�wB	�}B	��B	ÖB	ƨB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�
B	�
B	�
B	�B	�B	�B	�B	�B	�#B	�)B	�/B	�/B	�5B	�5B	�;B	�BB	�HB	�NB	�TB	�TB	�ZB	�`B	�fB	�mB	�mB	�sB	�sB	�sB	�sB	�sB	�sB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
+B
1B
	7B
	7B

=B

=B

=B
DB
DB
DB
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
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
"�B
#�B
$�B
&�B
&�B
&�B
'�B
'�B
'�B
'�B
'�B
(�B
(�B
)�B
)�B
+B
+B
,B
,B
-B
.B
.B
.B
.B
.B
.B
.B
/B
/B
/B
0!B
1'B
1'B
1'B
2-B
2-B
2-B
2-B
2-B
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
6FB
6FB
7LB
7LB
8RB
8RB
9XB
9XB
9XB
:^B
:^B
:^B
;dB
<jB
=qB
=qB
=qB
=qB
=qB
=qB
=qB
=qB
?}B
?}B
?}B
?}B
?}B
@�B
A�B
A�B
B�B
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
C�B
D�B
D�B
E�B
E�B
E�B
F�B
G�B
G�B
G�B
G�B
G�B
H�B
H�B
I�B
I�B
H�B
H�B
I�B
I�B
I�B
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
L�B
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
T�B
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
XB
XB
XB
YB
YB
YB
YB
ZB
ZB
[#B
[#B
[#B
[#B
[#B
\)B
\)B
\)B
]/B
]/B
^5B
_;B
_;B
_;B
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
cTB
dZB
dZB
dZB
e`B
e`B
ffB
ffB
ffB
ffB
ffB
ffB
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
l�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
n�B
o�B
o�B
o�B
o�B
o�B
o�B
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
s�B
s�B
s�B
t�B
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
u�B
u�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
x�B
x�B
x�B
x�B
x�B
x�B
y�B
y�B
y�B
y�B
z�B
z�B
z�B
z�B
z�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�B�B�B�B�B�B�3B�3B��B�SB��B��B��B��B�JB��BݲB��B�5B��B�=B�B�mB�B�B�5B�+B�BѝB��B�)B�RBǮB�B��B�OB��B�B�2B��B�eB�ZB��B��B��B�?B|�Bs�BlWBdB[=BS�BO�BI�BC{B>�B8�B,�B)yB'�B"hB!bB!|B \BQB	�B�B�FB��B�NBȴB��B�`B��B��B��B}<Bs�Bi_Bc�B\)BMjB:^B"B(B
�.B
��B
�3B
�B
�1B
өB
�oB
��B
�1B
��B
�B
�4B
i�B
T�B
N�B
G�B
<6B
&�B
�B
:B
	B	��B	�+B	�}B	��B	�FB	��B	�fB	ðB	��B	��B	��B	�YB	�B	zxB	x�B	tTB	pUB	mwB	j�B	bhB	^�B	XEB	L�B	E�B	:^B	49B	5tB	6�B	7�B	>(B	="B	>�B	<�B	6�B	0�B	'�B	�B	B	B	�B	B	�B	�B	�B	
=B	9B	 B�jB�B�MB�kB�B�)B�pBބB׍BϑB�1B�B��B��B�GB��B�=B�zB��B��B��B��B��B�B�B��B��B�	B�#B��B��B��B�TB��B��B��B~�B|�By�Bw�Bv+BuZBtnBshBqvBo5Bm]BlqBj�BhsBdZB^B[�B[�B[	B[�BZ�BZQBXyBU2BQ�BO�BN�BK�BH1BG�BFBC�BA�B?.B:�B8�B5?B1�B/�B.cB-B*�B)�B(>B&LB$@B"�B!B!bB \B 'B 'B B \B �B �B!�B#nB#�B#�B$�B$&B$B#�B$&B$ZB$&B%B$�B"hB BB5B BB!B%�B,�B0�B1B1B1�B1�B2�B3�B3�B33B3MB4B3�B2�B2�B2GB1AB0;B/�B0�B1�B1�B0�B0�B0UB0UB0UB0�B0�B0�B0B1�B1�B1�B1�B2B33B4B2B.�B-�B,qB+�B,"B-�B/�B1AB1�B0�B0�B0!B0;B/B/iB/iB3MB5�B7�B7�B8�B8�B8�B9�B;�B<�B=�B>�B?HBA;BCGBG_BHfBK�BN�BR�BW�BY�B[�B_�Bb�Be,Bi*Bl=Bq'Bt9ButByrB|jB~]B�;B�[B�{B�gB�tB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�&B�2B�*B�B�B�B�B�0B�eB��B�TB�tB�tB��B��B��B��B��B��B��B��B��B�'B�MB��B��B��B�B�B�B�B�B�B�&B�@B�mB�BܬB�B��B�B��B�B��B��B��B��B�B��B��B�B�B�6B�HB	uB	�B		�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	!-B	$B	'mB	-wB	1[B	1�B	6zB	9�B	;�B	@�B	@�B	A�B	C�B	D�B	F�B	G�B	IB	J�B	K�B	K�B	L�B	L�B	M�B	M�B	N�B	OB	OB	Q4B	TFB	V9B	X_B	\]B	`\B	bhB	bhB	b�B	dtB	e�B	f�B	g�B	iyB	i�B	i�B	i�B	k�B	l�B	o�B	q�B	q�B	s3B	y�B	{B	~B	B	�4B	�iB	�AB	�3B	�3B	�9B	�YB	�?B	�EB	�_B	��B	��B	�~B	�pB	�pB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�5B	�BB	�@B	�>B	�B	�"B	�]B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�(B	�.B	�B	�&B	�B	�B	��B	�B	�B	�B	�B	�B	�9B	�$B	�$B	�$B	�EB	�1B	�KB	�KB	�QB	�=B	�CB	�dB	�IB	�jB	�OB	�pB	�vB	�|B	�B	�nB	�B	�tB	�B	�B	�B	�B	�sB	�sB	�sB	�sB	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�	B	��B	�B	�B	�6B	�B	��B	�B
 B
 B
  B
 B
B
B
 B
 B
 B
'B
'B
-B
-B
3B
3B
SB
SB
?B
_B
_B
KB
	lB
	lB

XB

XB

XB
xB
^B
xB
jB
jB
pB
�B
�B
vB
vB
}B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
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
"B
# B
$B
%B
'B
'B
&�B
($B
(
B
(
B
($B
($B
)B
)B
*B
*B
+6B
+6B
,"B
,=B
-)B
.B
.B
.B
./B
./B
./B
./B
/5B
/OB
/iB
0;B
1AB
1AB
1AB
2-B
2GB
2GB
2aB
2aB
49B
4TB
4TB
49B
49B
4TB
4TB
5ZB
5tB
5tB
6zB
6`B
6`B
6`B
7�B
7fB
8lB
8lB
9�B
9rB
9rB
:xB
:�B
:�B
;�B
<�B
=qB
=qB
=qB
=�B
=�B
=�B
=�B
=�B
?�B
?}B
?�B
?�B
?�B
@�B
A�B
A�B
B�B
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
C�B
D�B
D�B
E�B
E�B
E�B
F�B
G�B
G�B
G�B
G�B
G�B
H�B
H�B
I�B
I�B
H�B
H�B
I�B
I�B
I�B
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
MB
MB
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
PB
QB
RB
RB
RB
S&B
SB
SB
SB
TB
TB
TB
T,B
UB
UB
U2B
VB
VB
VB
VB
WYB
W?B
X+B
XEB
X+B
X+B
Y1B
Y1B
Y1B
YeB
ZQB
Z7B
[=B
[=B
[WB
[WB
[=B
\]B
\]B
\xB
]IB
]~B
^OB
_VB
_VB
_VB
`vB
aHB
aHB
abB
abB
bhB
bhB
bhB
cnB
cnB
cnB
cnB
c�B
c�B
cnB
dZB
d�B
d�B
ezB
ezB
f�B
f�B
f�B
f�B
f�B
f�B
g�B
g�B
g�B
g�B
h�B
h�B
h�B
hsB
h�B
h�B
i�B
i�B
i�B
i�B
i�B
j�B
jB
jB
jB
jeB
j�B
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
m�B
m�B
m�B
m�B
n�B
o�B
o�B
o�B
o�B
o�B
o�B
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
s�B
s�B
s�B
t�B
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
u�B
u�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
x�B
x�B
x�B
x�B
x�B
x�B
y�B
y�B
y�B
y�B
z�B
z�B
z�B
z�B
z�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<���<m�h<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.02(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201908020032022019080200320220190802003202202211182139472022111821394720221118213947201908030016402019080300164020190803001640  JA  ARFMdecpA19c                                                                20190723004315  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20190722154326  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20190722154329  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20190722154330  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20190722154331  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20190722154331  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20190722154331  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20190722154331  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20190722154333  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20190722154333                      G�O�G�O�G�O�                JA  ARUP                                                                        20190722155913                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20190722153136  CV  JULD            G�O�G�O�F�y                 JM  ARCAJMQC2.0                                                                 20190801153202  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20190801153202  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20190802151640  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20221118123947  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20221123111508                      G�O�G�O�G�O�                