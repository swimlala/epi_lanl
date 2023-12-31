CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       ~2017-10-10T15:36:24Z creation;2017-10-10T15:36:27Z conversion to V3.1;2019-12-18T07:27:36Z update;2022-11-21T05:31:52Z update;     
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
_FillValue                 �  ]   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  pt   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tX   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �D   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �(   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ͘   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ݬ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �,   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �0   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �4   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �8   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �<   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �|   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20171010153624  20221123111507  4902148 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               sA   JA  I1_0397_115                     2C  Dd#NAVIS_A                         0397                            ARGO 011514                     863 @�,�[�ހ1   @�,��S�@;������d#:��1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�33@�  A   AffA@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8ffB?��BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��3D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111���@��\@�\)@�\)A{A?�A_�A�A��
A��
A��
A��
A��
A��
A��
A��
B�B�B�B�B'�B/�B8Q�B?�BG�BO�BW�B_�Bg�Bo�Bw�B�B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��qC��qC��qC��qC��qC��qC��qC��qC�
=C�
=C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qD ~�D ��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D	~�D	��D
~�D
��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��DxRD��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D ~�D ��D!~�D!��D"~�D"��D#~�D#��D$~�D$��D%~�D%��D&~�D&��D'~�D'��D(~�D(��D)~�D)��D*~�D*��D+~�D+��D,~�D,��D-~�D-��D.~�D.��D/~�D/��D0~�D0��D1~�D1��D2~�D2��D3~�D3��D4~�D4��D5~�D5��D6~�D6��D7~�D7��D8~�D8��D9~�D9��D:~�D:��D;~�D;��D<~�D<��D=~�D=��D>~�D>��D?~�D?��D@~�D@��DA~�DA��DB~�DB��DC~�DC��DD~�DD��DE~�DE��DF~�DF��DG~�DG��DH~�DH��DI~�DI��DJ~�DJ��DK~�DK��DL~�DL��DM~�DM��DN~�DN��DO~�DO��DP~�DP��DQ~�DQ��DR~�DR��DS~�DS��DT~�DT��DU~�DU��DV~�DV��DW~�DW��DX~�DX��DY~�DY��DZ~�DZ��D[~�D[��D\~�D\��D]~�D]��D^~�D^��D_~�D_��D`~�D`��Da~�Da��Db~�Db��Dc~�Dc��Dd~�Dd��De~�De��Df~�Df��Dg~�Dg��Dh~�Dh��Di~�Di��Dj~�Dj��Dk~�Dk��Dl~�Dl��Dm~�Dm��Dn~�Dn��Do~�Do��Dp~�Dp��Dq~�Dq��Dr~�Dr��Ds~�Ds��Dt~�Dt��Du~�Du��Dv~�Dv��Dw~�Dw��Dx~�Dx��Dy~�Dy��Dz~�Dz��D{~�D{��D|~�D|��D}~�D}��D~~�D~��D~�D��D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��)D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D¿\D��\D�?\D�\Dÿ\D��\D�?\D�\DĿ\D��\D�?\D�\Dſ\D��\D�?\D�\Dƿ\D��\D�?\D�\Dǿ\D��\D�?\D�\Dȿ\D��\D�?\D�\Dɿ\D��\D�?\D�\Dʿ\D��\D�?\D�\D˿\D��\D�?\D�\D̿\D��\D�?\D�\DͿ\D��\D�?\D�\Dο\D��\D�?\D�\DϿ\D��\D�?\D�\Dп\D��\D�?\D�\Dѿ\D��\D�?\D�\Dҿ\D��\D�?\D�\Dӿ\D��\D�?\D�\DԿ\D��\D�?\D�\Dտ\D��\D�?\D�\Dֿ\D��\D�?\D�\D׿\D��\D�?\D�\Dؿ\D��\D�?\D�\Dٿ\D��\D�?\D�\Dڿ\D��\D�?\D�\Dۿ\D��\D�?\D�\Dܿ\D��\D�?\D�\Dݿ\D��\D�?\D�\D޿\D��\D�?\D�\D߿\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D��\D��\D�?\D�\D�\D��\D�?\D�\D�D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D��)111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111��^AсAсA�|�A�|�Aч+AхAч+Aщ7Aщ7Aщ7Aщ7AыDAыDAыDAэPAхAщ7AыDA�\)A�A�A���A�oA���A�(�A��A���A��DA�9XA���A���A��A��RA��^A�;dA�A��PA�A���A���A� �A�"�A���A�33A��mA���A�7LA�^5A��7A�ffA�dZA�`BA���A�z�A�33A��HA�-A��+A�+A�v�A���A�-A���A�oA���A�VA��\A�1'A���A�A�ȴA��DA���A�t�A�t�A�A�A��A��TA�C�A��^A�33A�t�A���A�~�A�r�A�&�A��^A� �A%A|�A|VA{�mA{?}Az��AzbNAz  Ax�9Ax�Aw33AvA�Au33At��As�mAr��Ap�/An�9Ak�mAk%Aj�!AjI�Ai�AiAi��Ah�yAfVAe;dAd^5AcdZAbJAa`BA`~�A_
=A]��A]K�A\Q�A[��A[�AY��AYoAX�AVZAUG�AT1'AR�AP�APbAO�AN5?AL�\AK�mAKhsAJȴAJ9XAI��AHn�AG�PAF�AE+AD�AC�wAB��AA�#A@��A?�A>�A=�^A;��A:��A9�mA8�A7ƨA6VA3��A2�A2bA1�A1�
A1�-A1x�A1+A0�yA0�`A0ĜA01'A/\)A.�9A.ZA-�TA-�A+�-A*�RA)�;A)�A)K�A(ĜA(��A(9XA'%A%�A$�\A#��A#�7A#|�A#7LA"�A"jA!&�A $�AXA�DAp�A^5A�jA�A�-A|�Al�AVA�HA�A~�A=qA��AG�A��A�!AbNA=qA�#A�PAhsA?}AoA�RA�A33A�jA�DA^5A{A�#A�-AO�A�Al�A7LA�RA�AĜAA
��A
(�A	��AĜAoAƨAr�A%A�A�
A��A7LA �A  �@��@���@�;d@�&�@�K�@��@�j@�@�`B@@�p�@��@�o@�-@�&�@�z�@�t�@�^@�9X@��@�t�@���@�n�@��/@�dZ@���@�z�@�Q�@�|�@ָR@���@Ցh@�O�@��/@�1'@��;@�dZ@��@�ȴ@�=q@�G�@�(�@Ο�@̴9@�  @�\)@�@���@�p�@ȴ9@�Q�@ǶF@�-@���@��-@��j@�S�@�~�@��@��@��^@��-@���@�`B@�&�@�A�@�b@��;@���@�33@���@�=q@�-@���@� �@��@��@���@���@��@�dZ@�S�@�C�@�"�@�
=@��H@�ȴ@���@��+@�@��@�7L@�v�@���@��y@���@�@�Z@�|�@���@�p�@���@��9@��D@�bN@���@�$�@���@��D@�9X@��@�1@��m@�@��`@�  @�l�@��@�ȴ@�^5@��^@�V@�r�@���@��w@��P@�\)@�o@�ȴ@�v�@�$�@�@��^@��h@��7@��@�X@���@�Z@��@�l�@�@��!@�v�@�5?@��-@�G�@���@���@��u@�I�@���@��F@�\)@��@��H@�=q@���@�hs@��@��9@��@�I�@�  @�$�@��@�x�@�O�@�?}@�V@���@�Ĝ@��u@��u@�z�@�Z@�A�@��@��
@���@�\)@�@���@�M�@��#@���@��@�7L@��/@���@���@�r�@��@�;@�w@
=@~�+@~@}�T@}��@}?}@|��@|�D@|Z@|1@{�F@{dZ@{@z��@z�\@z=q@z�@y��@y�@y��@y�7@x�9@w|�@w�P@w��@w
=@u@t�j@t�@sdZ@s"�@r�H@r^5@q�@q��@qx�@qG�@p��@p��@pbN@o��@nȴ@l1@k"�@j��@i�@hĜ@h  @g��@g\)@g�@f�y@fȴ@f�R@f��@fv�@f5?@f$�@f{@f{@f@e�@e�T@e��@e�-@e��@e�h@e�@e�@ep�@e`B@e�@eV@d��@eV@d�@d�/@d��@dZ@c�m@c��@cdZ@c"�@b��@bn�@b^5@b=q@a��@aX@a7L@`��@`��@`b@_��@_l�@_\)@_+@^�@^V@^@]`B@]V@]V@\�/@\�@\j@\I�@\(�@[��@[�F@[��@[dZ@["�@Zn�@Y��@Y�^@Yx�@X��@XbN@X1'@X  @W��@WK�@W
=@Vff@U�T@U��@T�@T��@T��@TI�@S�@S@RJ@P�`@P�9@P��@P�@Pr�@Pr�@Pr�@PQ�@P  @O|�@O
=@N�@Nȴ@N�y@N�y@N��@Nff@M��@L�@L�@L�/@L��@L�j@L�@L�D@Lz�@Lz�@Lj@LI�@L(�@L�@K�
@K��@Kt�@K"�@K@J�H@J��@J-@I��@Ix�@IG�@H��@HQ�@H �@H  @G�;@G��@GK�@FV@E`B@Dz�@D(�@C��@C@B�H@B�H@B�!@B�\@B^5@B�@A�#@Ax�@@�`@@�9@@��@@��@@�u@@r�@@1'@@ �@@b@?�;@?�P@?|�@?�@>��@>�y@>�+@>{@=��@=�@<�@<1@;t�@;C�@;"�@;"�@:��@:^5@9�#@9�7@9G�@9%@8Ĝ@8�u@8Q�@7�P@6ȴ@6V@6{@6@6@5��@5`B@5�@4�@4��@4Z@4�@3�
@3��@3S�@3o@2~�@2-@17L@0��@0��@0��@0Ĝ@0�@0A�@/�w@/�P@/\)@.��@.ȴ@.ȴ@.ȴ@.��@.E�@.{@-�T@-�h@-V@,�/@,��@,�j@,I�@+�
@+��@+��@+�@+t�@+33@*J@)hs@(�`@(��@(�u@(�@(�@(r�@(Q�@'�w@'K�@'+@'�@&�y@&ȴ@&��@&v�@&V@%�@%V@$��@$�j@$�D@$I�@$(�@$�@$1@#�m@#�
@#�F@#��@#��@#��@#�@#t�@#C�@"�@"�\@"M�@"J@!�^@!x�@!7L@!&�@!%@ Ĝ@ �u@ bN@ Q�@ b@�@��@\)@ȴ@v�@ff@�@�@�T@��@`B@�/@��@�@z�@9X@�
@��@��@��@��@"�@��@&�@�9@r�@ �@b@  @�@�@|�@|�@|�@|�@|�@|�@l�@+@ff@$�@�@�-@�h@�@p�@p�@O�@�@��@(�@1@�m@�
@S�@�\@=q@�@��@G�@%@�9@�@r�@Q�@Q�@A�@1'@A�@1'@ �@1'@1'@1'@1'@1'@1'@b@�@�;@�w@��@��@�+@V@�@?}@��@�D@z�@Z@9X@(�@(�@(�@�@�@�@1@�
@��@��@�@t�@C�@
��@
^5@
-@	��@	�^@	hs@	&�@�`@  @��@�P@�P@�P@�P@�P@�P@�P@��@�P@l�@l�@|�@l�@\)@K�@;d@+@�@
=@ȴ@{@�h@p�@`B@`B@O�@O�@O�@?}@?}@�@��@�j@z�@Z@I�@9X@1@�m@�
@�
@�
@�F@�@S�@"�@@��@n�@=q@-@J@��@��@�@�@�@�@�@��@��@��@�^@�7@hs@X@G�@7L@&�@�@�@�@%@%@ ��@ ��@ ��@ ��@ �@ r�@ bN@ 1'@   ?���?�\)?��?��?���?�v�?�5??�{?���?��-?��D?�C�?���?��?��^?�7L?���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111��^AсAсA�|�A�|�Aч+AхAч+Aщ7Aщ7Aщ7Aщ7AыDAыDAыDAэPAхAщ7AыDA�\)A�A�A���A�oA���A�(�A��A���A��DA�9XA���A���A��A��RA��^A�;dA�A��PA�A���A���A� �A�"�A���A�33A��mA���A�7LA�^5A��7A�ffA�dZA�`BA���A�z�A�33A��HA�-A��+A�+A�v�A���A�-A���A�oA���A�VA��\A�1'A���A�A�ȴA��DA���A�t�A�t�A�A�A��A��TA�C�A��^A�33A�t�A���A�~�A�r�A�&�A��^A� �A%A|�A|VA{�mA{?}Az��AzbNAz  Ax�9Ax�Aw33AvA�Au33At��As�mAr��Ap�/An�9Ak�mAk%Aj�!AjI�Ai�AiAi��Ah�yAfVAe;dAd^5AcdZAbJAa`BA`~�A_
=A]��A]K�A\Q�A[��A[�AY��AYoAX�AVZAUG�AT1'AR�AP�APbAO�AN5?AL�\AK�mAKhsAJȴAJ9XAI��AHn�AG�PAF�AE+AD�AC�wAB��AA�#A@��A?�A>�A=�^A;��A:��A9�mA8�A7ƨA6VA3��A2�A2bA1�A1�
A1�-A1x�A1+A0�yA0�`A0ĜA01'A/\)A.�9A.ZA-�TA-�A+�-A*�RA)�;A)�A)K�A(ĜA(��A(9XA'%A%�A$�\A#��A#�7A#|�A#7LA"�A"jA!&�A $�AXA�DAp�A^5A�jA�A�-A|�Al�AVA�HA�A~�A=qA��AG�A��A�!AbNA=qA�#A�PAhsA?}AoA�RA�A33A�jA�DA^5A{A�#A�-AO�A�Al�A7LA�RA�AĜAA
��A
(�A	��AĜAoAƨAr�A%A�A�
A��A7LA �A  �@��@���@�;d@�&�@�K�@��@�j@�@�`B@@�p�@��@�o@�-@�&�@�z�@�t�@�^@�9X@��@�t�@���@�n�@��/@�dZ@���@�z�@�Q�@�|�@ָR@���@Ցh@�O�@��/@�1'@��;@�dZ@��@�ȴ@�=q@�G�@�(�@Ο�@̴9@�  @�\)@�@���@�p�@ȴ9@�Q�@ǶF@�-@���@��-@��j@�S�@�~�@��@��@��^@��-@���@�`B@�&�@�A�@�b@��;@���@�33@���@�=q@�-@���@� �@��@��@���@���@��@�dZ@�S�@�C�@�"�@�
=@��H@�ȴ@���@��+@�@��@�7L@�v�@���@��y@���@�@�Z@�|�@���@�p�@���@��9@��D@�bN@���@�$�@���@��D@�9X@��@�1@��m@�@��`@�  @�l�@��@�ȴ@�^5@��^@�V@�r�@���@��w@��P@�\)@�o@�ȴ@�v�@�$�@�@��^@��h@��7@��@�X@���@�Z@��@�l�@�@��!@�v�@�5?@��-@�G�@���@���@��u@�I�@���@��F@�\)@��@��H@�=q@���@�hs@��@��9@��@�I�@�  @�$�@��@�x�@�O�@�?}@�V@���@�Ĝ@��u@��u@�z�@�Z@�A�@��@��
@���@�\)@�@���@�M�@��#@���@��@�7L@��/@���@���@�r�@��@�;@�w@
=@~�+@~@}�T@}��@}?}@|��@|�D@|Z@|1@{�F@{dZ@{@z��@z�\@z=q@z�@y��@y�@y��@y�7@x�9@w|�@w�P@w��@w
=@u@t�j@t�@sdZ@s"�@r�H@r^5@q�@q��@qx�@qG�@p��@p��@pbN@o��@nȴ@l1@k"�@j��@i�@hĜ@h  @g��@g\)@g�@f�y@fȴ@f�R@f��@fv�@f5?@f$�@f{@f{@f@e�@e�T@e��@e�-@e��@e�h@e�@e�@ep�@e`B@e�@eV@d��@eV@d�@d�/@d��@dZ@c�m@c��@cdZ@c"�@b��@bn�@b^5@b=q@a��@aX@a7L@`��@`��@`b@_��@_l�@_\)@_+@^�@^V@^@]`B@]V@]V@\�/@\�@\j@\I�@\(�@[��@[�F@[��@[dZ@["�@Zn�@Y��@Y�^@Yx�@X��@XbN@X1'@X  @W��@WK�@W
=@Vff@U�T@U��@T�@T��@T��@TI�@S�@S@RJ@P�`@P�9@P��@P�@Pr�@Pr�@Pr�@PQ�@P  @O|�@O
=@N�@Nȴ@N�y@N�y@N��@Nff@M��@L�@L�@L�/@L��@L�j@L�@L�D@Lz�@Lz�@Lj@LI�@L(�@L�@K�
@K��@Kt�@K"�@K@J�H@J��@J-@I��@Ix�@IG�@H��@HQ�@H �@H  @G�;@G��@GK�@FV@E`B@Dz�@D(�@C��@C@B�H@B�H@B�!@B�\@B^5@B�@A�#@Ax�@@�`@@�9@@��@@��@@�u@@r�@@1'@@ �@@b@?�;@?�P@?|�@?�@>��@>�y@>�+@>{@=��@=�@<�@<1@;t�@;C�@;"�@;"�@:��@:^5@9�#@9�7@9G�@9%@8Ĝ@8�u@8Q�@7�P@6ȴ@6V@6{@6@6@5��@5`B@5�@4�@4��@4Z@4�@3�
@3��@3S�@3o@2~�@2-@17L@0��@0��@0��@0Ĝ@0�@0A�@/�w@/�P@/\)@.��@.ȴ@.ȴ@.ȴ@.��@.E�@.{@-�T@-�h@-V@,�/@,��@,�j@,I�@+�
@+��@+��@+�@+t�@+33@*J@)hs@(�`@(��@(�u@(�@(�@(r�@(Q�@'�w@'K�@'+@'�@&�y@&ȴ@&��@&v�@&V@%�@%V@$��@$�j@$�D@$I�@$(�@$�@$1@#�m@#�
@#�F@#��@#��@#��@#�@#t�@#C�@"�@"�\@"M�@"J@!�^@!x�@!7L@!&�@!%@ Ĝ@ �u@ bN@ Q�@ b@�@��@\)@ȴ@v�@ff@�@�@�T@��@`B@�/@��@�@z�@9X@�
@��@��@��@��@"�@��@&�@�9@r�@ �@b@  @�@�@|�@|�@|�@|�@|�@|�@l�@+@ff@$�@�@�-@�h@�@p�@p�@O�@�@��@(�@1@�m@�
@S�@�\@=q@�@��@G�@%@�9@�@r�@Q�@Q�@A�@1'@A�@1'@ �@1'@1'@1'@1'@1'@1'@b@�@�;@�w@��@��@�+@V@�@?}@��@�D@z�@Z@9X@(�@(�@(�@�@�@�@1@�
@��@��@�@t�@C�@
��@
^5@
-@	��@	�^@	hs@	&�@�`@  @��@�P@�P@�P@�P@�P@�P@�P@��@�P@l�@l�@|�@l�@\)@K�@;d@+@�@
=@ȴ@{@�h@p�@`B@`B@O�@O�@O�@?}@?}@�@��@�j@z�@Z@I�@9X@1@�m@�
@�
@�
@�F@�@S�@"�@@��@n�@=q@-@J@��@��@�@�@�@�@�@��@��@��@�^@�7@hs@X@G�@7L@&�@�@�@�@%@%@ ��@ ��@ ��@ ��@ �@ r�@ bN@ 1'@   ?���?�\)?��?��?���?�v�?�5??�{?���?��-?��D?�C�?���?��?��^?�7L?���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111T�B5?B5?B6FB6FB5?B5?B6FB6FB6FB6FB6FB6FB6FB6FB5?B5?B5?B5?B33B1'B-B�B��B��B��B�hB�=B�Br�Be`BaHB\)BT�BO�BM�BK�BJ�BI�BG�BE�BA�B?}B;dB9XB7LB33B,B!�B�BhB+B��B�B�ZB�B��B��BȴB�}B�LB�B��B��B��B�%Bn�BiyBbNB\)BYBVBN�BH�B<jB,B�BVB%B
��B
�B
�/B
��B
��B
��B
ɺB
��B
�FB
�{B
�B
�B
}�B
x�B
t�B
q�B
n�B
e`B
`BB
XB
O�B
F�B
@�B
9XB
.B
�B
JB	��B	��B	��B	��B	�B	�B	�B	�yB	�#B	��B	��B	ƨB	�wB	�^B	�?B	�B	��B	��B	��B	��B	�uB	�DB	�%B	�B	w�B	q�B	jB	_;B	W
B	P�B	J�B	E�B	=qB	9XB	7LB	33B	0!B	,B	$�B	 �B	�B	uB	VB	DB	%B	B��B��B�B�B�`B�NB�;B�B��B��BĜB��B��B��B�}B�}B�wB�qB�jB�jB�dB�XB�LB�?B�3B�'B�B�B��B��B��B��B��B��B��B��B��B��B�{B��B��B��B�{B�hB�VB�DB�7B�+B�B�B~�Bz�Bx�Bw�Bw�Bw�Bv�Bu�Bt�Bs�Bo�Bl�BjBiyBiyBhsBgmBgmBffBffBe`BdZBcTBbNBaHBaHBaHBaHBaHB`BB_;B`BBaHB`BB`BB^5B]/B\)BZBXBVBVBW
BT�BR�BP�BN�BM�BL�BI�BF�BE�BC�BB�BA�B@�B?}B>wB<jB:^B9XB7LB6FB6FB6FB5?B5?B5?B49B49B49B33B49B33B33B2-B2-B49B49B49B5?B5?B6FB6FB6FB7LB7LB8RB8RB8RB8RB7LB7LB6FB5?B49B33B2-B2-B0!B/B.B,B+B)�B&�B$�B$�B%�B'�B(�B(�B)�B)�B)�B+B,B0!B1'B1'B1'B2-B1'B0!B9XB<jB=qB>wB>wB?}B?}B?}B?}B?}B?}B?}B?}B?}B?}B?}B?}BA�BE�BE�BE�BK�BM�BN�BP�BW
B[#B_;BdZBgmBhsBhsBhsBhsBn�Bs�Bt�Bu�Bv�Bv�Bv�B�B�B�+B�1B�7B�=B�JB�\B�oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�-B�3B�9B�FB�RB�dB�wB��BBÖBƨBȴB��B��B��B��B��B�B�#B�)B�5B�HB�NB�TB�ZB�B�B��B��B��B��B��B��B��B��B��B��B��B��B	  B	B	B	%B	1B	
=B	PB	\B	bB	uB	�B	�B	�B	�B	�B	�B	�B	"�B	%�B	)�B	+B	-B	/B	0!B	33B	49B	5?B	6FB	8RB	:^B	;dB	<jB	>wB	?}B	?}B	?}B	@�B	B�B	F�B	L�B	L�B	K�B	N�B	T�B	ZB	^5B	aHB	cTB	dZB	gmB	iyB	jB	k�B	l�B	n�B	p�B	q�B	s�B	w�B	�B	�1B	�=B	�JB	�\B	�hB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�-B	�3B	�?B	�^B	�dB	�dB	�jB	�qB	�}B	��B	��B	��B	B	ĜB	ƨB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�B	�#B	�/B	�;B	�BB	�HB	�NB	�NB	�ZB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
%B
+B
+B
+B
+B
1B

=B
JB
VB
\B
\B
\B
\B
\B
bB
bB
bB
hB
oB
oB
uB
uB
uB
uB
uB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
"�B
"�B
#�B
$�B
%�B
%�B
%�B
%�B
&�B
&�B
'�B
'�B
'�B
(�B
(�B
)�B
)�B
)�B
)�B
+B
,B
-B
.B
.B
.B
.B
.B
/B
/B
0!B
0!B
1'B
1'B
1'B
1'B
2-B
33B
33B
33B
49B
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
7LB
9XB
:^B
;dB
<jB
<jB
<jB
<jB
<jB
<jB
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
A�B
A�B
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
C�B
C�B
D�B
D�B
D�B
D�B
E�B
E�B
F�B
F�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
J�B
J�B
K�B
K�B
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
O�B
O�B
N�B
N�B
O�B
P�B
Q�B
R�B
R�B
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
T�B
T�B
VB
W
B
XB
XB
XB
XB
XB
XB
XB
XB
YB
YB
ZB
ZB
ZB
ZB
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
`BB
`BB
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
`BB
`BB
`BB
`BB
aHB
bNB
bNB
bNB
cTB
dZB
dZB
e`B
e`B
e`B
e`B
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
gmB
gmB
hsB
hsB
hsB
iyB
iyB
iyB
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
l�B
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
m�B
n�B
o�B
o�B
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
q�B
q�B
q�B
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
s�B
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
w�B
w�B
w�B
w�B
w�B
w�B
x�B
x�B
x�B
x�B
y�B
z�B
z�B
{�B
{�B
|�B
|�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111T�B5?B5?B6FB6+B5?B5?B6FB6FB6FB6FB6FB6FB6FB6FB5ZB5?B5tB5�B4B3�B4B2|B�&B�hB�7B�aB�pB��B}Bg�Bc�B^BVBP�BN�BMBL0BL0BJ�BG�BBuB@�B<6B:*B8�B5�B/�B$tB�B�B	RB�B��B�B��B�aB��B�=B��B�	B�wB�`B��B�B�Bo�Bj�Bc�B\�BY�BWsBP}BKDB?HB.�B;B�B�B
��B
�B
��B
�4B
�HB
��B
�DB
�3B
�0B
��B
��B
��B
~�B
yrB
uZB
r|B
pB
fLB
abB
YKB
QB
G�B
A�B
;B
0�B
 vB
(B
  B	�xB	�RB	�+B	�B	�GB	�B	�=B	ܒB	�,B	�"B	�1B	�}B	��B	��B	�cB	��B	��B	��B	��B	��B	�dB	��B	��B	yXB	sMB	l�B	aB	XEB	R:B	L0B	G_B	>]B	:*B	8B	4B	1'B	-wB	&B	!�B	�B	�B	(B	�B	_B	�B��B��B�3B�B��B�nB�B��B�9BЗB�B�B��B��B��B��B��B��B��B��B�PB�xB�B��B�B�|B��B�CB��B�RB��B��B�NB��B�VB�CB�7B�eB��B��B�
B�$B��B�B��B��B��B��B��B��B�B{dBy$BxBxRBxBw2Bv+Bu?BtTBpUBmBkBjBi�Bh�Bg�Bg�Bf�Bf�BfBezBd@BcBa�Ba�Ba�Ba�Ba�Ba-B`�Ba-Ba�Ba-BabB_�B^�B]�BZ�BX�BW�BX_BX�BV�BT�BRBOBBNVBM�BJ�BG_BF�BD�BD3BB�BA�B@�B?�B=�B;B:�B8�B7fB7B6�B6B5�B6+B5�B5tB5�B5%B4�B3�B4nB3hB3�B4�B4�B4�B5�B5�B6�B6�B6�B7�B7�B8�B8�B8�B8�B8B88B7�B6zB4�B3�B2�B2�B1B/�B.�B,�B,=B+kB)_B'�B%�B&�B(>B)*B)*B*B*0B*KB+QB,�B0UB1vB1�B1�B2�B2GB2�B:DB<�B=�B>�B>�B?�B?�B?�B?�B?�B?�B?�B?�B?�B?�B?�B?�BBBF�BG�BG_BLJBN<BO�BRBW�B[�B`Bd�Bg�Bh�Bh�BiBi�BoiBtBuBu�Bv�BwLBx8B��B��B��B��B��B��B��B��B��B�B��B��B��B��B�B�B�B�B�B�B��B��B�B�@B�fB�B�aB��B��B�zB��B��B��B��B��B��B��B�B�B�0B�B�HB�hB�mB�WB�xBބB�B�B��B�B��B�B��B��B�B��B�	B�B�B�B�B�B�BB�HB	 4B	AB	aB	tB	�B	
�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	B	#B	&2B	*0B	+6B	-CB	/5B	0UB	3MB	4nB	5tB	6zB	8�B	:xB	;B	<�B	>�B	?�B	?�B	?�B	@�B	B�B	GB	L�B	L�B	LB	OvB	UgB	ZkB	^jB	abB	c�B	d�B	g�B	i�B	j�B	k�B	l�B	n�B	p�B	q�B	tTB	x�B	�mB	�fB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�$B	�B	�B	�)B	�IB	�;B	�GB	�MB	�tB	�xB	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	�B	�B	�4B	�&B	�B	�2B	�9B	�?B	�+B	�+B	�KB	�7B	�QB	�WB	�dB	�VB	�vB	�bB	�B	�B	�B	�B	��B	��B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	�B	�B	�B	�B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�(B	�B	�B	�B
 B
 B
 B
 B
;B
AB
-B
3B
MB
SB
?B
EB
EB
EB
�B
�B

�B
�B
�B
�B
�B
vB
\B
vB
}B
}B
}B
�B
�B
�B
�B
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
 �B
 �B
!�B
!�B
!�B
# B
# B
$&B
%B
%�B
%�B
%�B
%�B
'B
'B
(
B
(
B
($B
)B
(�B
*B
*B
*B
*0B
+6B
,WB
-)B
.B
.B
./B
.IB
./B
/iB
/5B
0;B
0;B
1[B
1AB
1'B
1AB
2GB
3hB
3hB
3MB
4�B
5ZB
5ZB
5ZB
5tB
6zB
7fB
7LB
7fB
7fB
7�B
7�B
9�B
:�B
;�B
<jB
<jB
<jB
<�B
<�B
<�B
=�B
>�B
>�B
>�B
>�B
?�B
?�B
?�B
?�B
?�B
A�B
A�B
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
C�B
C�B
D�B
D�B
D�B
D�B
E�B
E�B
F�B
F�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
J�B
J�B
K�B
K�B
L�B
L�B
L�B
MB
MB
M�B
M�B
M�B
N�B
OB
N�B
O�B
O�B
N�B
OBB
PHB
Q4B
R B
SB
SB
S�B
S�B
TB
TB
UB
T�B
T�B
UB
T�B
T�B
UB
U2B
V9B
W$B
XEB
XEB
X+B
XB
XB
XB
X+B
XEB
Y1B
YeB
Z7B
ZQB
Z7B
ZQB
[�B
\CB
]IB
]IB
]IB
^OB
^jB
_VB
_VB
_VB
_;B
_;B
`\B
`BB
_;B
`\B
`BB
`BB
`BB
`BB
`BB
`\B
`\B
`vB
`\B
`\B
`vB
`vB
a�B
bhB
b�B
b�B
c�B
dtB
dtB
ezB
e�B
e`B
e`B
e`B
e`B
ezB
e`B
ezB
ezB
ezB
f�B
ffB
f�B
f�B
f�B
g�B
g�B
h�B
h�B
h�B
i�B
i�B
i�B
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
l�B
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
m�B
n�B
o�B
o�B
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
q�B
q�B
q�B
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
s�B
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
w�B
xB
w�B
w�B
w�B
w�B
x�B
x�B
x�B
y	B
zB
z�B
{B
|B
|B
|�B
|�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<�	<�3�<#�
<#�
<#�
<#�
<#�
<%zx<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.02(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201710210032532017102100325320171021003253202211182132092022111821320920221118213209201804031937392018040319373920180403193739  JA  ARFMdecpA19c                                                                20171011003509  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20171010153624  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20171010153625  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20171010153625  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20171010153626  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20171010153626  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20171010153626  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20171010153626  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20171010153626  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20171010153627                      G�O�G�O�G�O�                JA  ARUP                                                                        20171010155543                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20171010153358  CV  JULD            G�O�G�O�F�e                JM  ARCAJMQC2.0                                                                 20171020153253  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20171020153253  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180403103739  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200114171534                      G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20221118123209  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20221123111507                      G�O�G�O�G�O�                