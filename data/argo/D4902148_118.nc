CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       ~2017-11-09T15:36:00Z creation;2017-11-09T15:36:03Z conversion to V3.1;2019-12-18T07:26:58Z update;2022-11-21T05:31:43Z update;     
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
_FillValue                 �  ]d   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  aP   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �,   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �\   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ޴   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �D   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �D   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �D   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �D   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �$   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �(   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �8   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �<   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �@   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �DArgo profile    3.1 1.2 19500101000000  20171109153600  20221123111507  4902148 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               vA   JA  I1_0397_118                     2C  Dd�NAVIS_A                         0397                            ARGO 011514                     863 @�4 )�� 1   @�4 �$�@:������d�c�A 1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @@  @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+fD+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DGfDG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D��31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111�@>�R@~�R@�\)@�\)A�A?�A_�A�A��
A��
A��
A��
A��
A��
A��
A��
B�B�B�B�B'�B/�B7�B?�BG�BO�BW�B_�Bg�Bo�Bw�B�B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B�(�B���B���B���B���B���B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qD ~�D ��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D	~�D	��D
~�D
��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D ~�D ��D!~�D!��D"~�D"��D#~�D#��D$~�D$��D%~�D%��D&~�D&��D'~�D'��D(~�D(��D)~�D)��D*~�D+D+~�D+��D,~�D,��D-~�D-��D.~�D.��D/~�D/��D0~�D0��D1~�D1��D2~�D2��D3~�D3��D4~�D4��D5~�D5��D6~�D6��D7~�D7��D8~�D8��D9~�D9��D:~�D:��D;~�D;��D<~�D<��D=~�D=��D>~�D>��D?~�D?��D@~�D@��DA~�DA��DB~�DB��DC~�DC��DD~�DD��DE~�DE��DF~�DGDG~�DG��DH~�DH��DI~�DI��DJ~�DJ��DK~�DK��DL~�DL��DM~�DM��DN~�DN��DO~�DO��DP~�DP��DQ~�DQ��DR~�DR��DS~�DS��DT~�DT��DU~�DU��DV~�DV��DW~�DW��DX~�DX��DY~�DY��DZ~�DZ��D[~�D[��D\~�D\��D]~�D]��D^~�D^��D_~�D_��D`~�D`��Da~�Da��Db~�Db��Dc~�Dc��Dd~�Dd��De~�De��Df~�Df��Dg~�Dg��Dh~�Dh��Di~�Di��Dj~�Dj��Dk~�Dk��Dl~�Dl��Dm~�Dm��Dn~�Dn��Do~�Do��Dp~�Dp��Dq~�Dq��Dr~�Dr��Ds~�Ds��Dt~�Dt��Du~�Du��Dv~�Dv��Dw~�Dw��Dx~�Dx��Dy~�Dy��Dz~�Dz��D{~�D{��D|~�D|��D}~�D}��D~~�D~��D~�D��D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D¿\D��\D�?\D�\Dÿ\D��\D�?\D�\DĿ\D��\D�?\D�\Dſ\D��\D�?\D�\Dƿ\D��\D�?\D�\Dǿ\D��\D�?\D�\Dȿ\D��\D�?\D�\Dɿ\D��\D�?\D�\Dʿ\D��\D�?\D�\D˿\D��\D�?\D�\D̿\D��\D�?\D�\DͿ\D��\D�?\D�\Dο\D��\D�?\D�\DϿ\D��\D�?\D�\Dп\D��\D�?\D�\Dѿ\D��\D�?\D�\Dҿ\D��\D�?\D�\Dӿ\D��\D�?\D�\DԿ\D��\D�?\D�\Dտ\D��\D�?\D�\Dֿ\D��\D�?\D�\D׿\D��\D�?\D�\Dؿ\D��\D�?\D�\Dٿ\D��\D�?\D�\Dڿ\D��\D�?\D�\Dۿ\D��\D�?\D�\Dܿ\D��\D�?\D�\Dݿ\D��\D�?\D�\D޿\D��\D�?\D�\D߿\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D��\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D���D�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111119A�S�A�XA�XA�VA�S�A�O�A�"�A�VA���A���A���A��A��`A���AļjAĕ�A�hsA�E�A�1'A��A�A��A��mA�K�A�A\A�v�A��A�+A�r�A���A��FA��9A��A��A�r�A��yA�bNA�O�A�1'A�Q�A�hsA�t�A��A���A�ZA�bNA�S�A�Q�A�ffA�VA�ĜA��`A�n�A��hA��A�I�A�S�A��A���A�A���A�5?A�Q�A�hsA�hsA��A�S�A�?}A�\)A���A��A�O�A�(�A�
=A���A� �A��9A�p�A��`A�A�K�A�&�A�1A"�A~�!A~�+A~9XA}�A}
=A|5?A{\)AzbNAx�DAt�At�+AtbNAt1AshsAs
=Aq;dApn�Am��Ag"�AfZAf1'Ae|�Ad�AdJAc��Ac�Ad�Ad1'Ad=qAct�Ac�AbĜAb�Aa&�A_��A^I�A]x�A]G�A\�uAZjAY��AX��AX��AX=qAW��AV��ATȴAS�PASdZAQ�AP��AO�wAM��AL~�AJȴAIt�AH9XAG�7AG�AF��AE�wAD�yAD �ACx�ABn�AA�PAA�A@��A@��A@bNA?�-A>�!A<=qA:Q�A9�mA8�`A8M�A7�mA7A7�A6�uA5XA41'A3�^A2�yA0�A/��A.jA,��A+�FA)��A)|�A)VA(�!A'�PA'XA'&�A&��A&��A&n�A&(�A$5?A#�A#�A#�;A#�hA"�A"A�A!��A!C�A!VA A�A��AA�A��Ar�A9XAbA��A�A�-AoAM�A�;AdZA�TA�Az�A�TA&�A�AXA&�A�PA�9A�DAI�AJA�
A"�A�yAjA�A	/AC�A?}A��An�AJA?}A��A�AI�A��Ax�A �@���@�G�@��9@�I�@�~�@�(�@�O�@�w@��@��D@�(�@�dZ@�!@�@�p�@��`@�bN@�1@��@���@�t�@�o@ꗍ@�~�@�E�@��@陚@�@�O�@��@��/@�Q�@�|�@�9@�-@ܬ@���@�l�@�"�@�O�@�n�@�@Ցh@Ձ@Ձ@Ձ@��@җ�@�/@���@�@���@�Z@�
=@��@���@�S�@�5?@�Q�@þw@�\)@��@�5?@��@��m@�C�@��y@��#@���@�=q@��9@�S�@�v�@�7L@�r�@�  @�l�@��@��@�~�@��@��-@�`B@�/@� �@�$�@�5?@�X@��@�j@�A�@���@��F@���@�S�@�~�@�-@�J@��@��h@�?}@�V@�%@�%@��@��j@�Q�@���@�t�@�~�@���@�+@��+@���@�K�@��@�
=@��y@���@�v�@���@�G�@��`@�n�@��@���@�bN@�bN@�Z@�Q�@�Z@�Z@�A�@�(�@�(�@��m@���@���@��-@���@��@��@��F@���@�\)@�;d@�"�@�@���@�~�@�@�?}@��D@�j@�A�@�1'@��@���@���@��P@�;d@��H@���@���@��\@�V@���@�I�@�
=@��!@��+@�n�@�V@�-@��@�@���@��@�G�@��@��@��u@��@�A�@���@��R@�M�@��h@�j@�b@|�@+@~�R@~�+@}@}/@{��@z�@y��@y��@yx�@xĜ@xbN@xQ�@xb@w�@w
=@v5?@t�@t�D@s��@r��@r^5@rJ@qG�@p�`@p�u@pQ�@p �@o�@o��@o�@o��@o|�@ol�@oK�@o;d@o;d@o+@o+@o�@o
=@n�+@nff@n5?@n@m��@m��@m�-@m��@m��@m�h@m�@mp�@m�@l�@l�j@lz�@lZ@lZ@lI�@l9X@l(�@l�@l�@kƨ@kt�@k"�@jM�@i�^@i�7@i7L@hĜ@f�y@e�@d(�@c�F@cC�@b�H@b~�@bJ@a��@ax�@a&�@`��@`r�@` �@_�@_;d@^�R@^5?@]�@]/@\��@[��@Z��@Z=q@ZJ@Y�@Y�^@Y�7@Yhs@Y7L@Y%@X��@X��@X�u@XbN@X �@W�;@Wl�@V�@Vv�@Vv�@Vv�@Vff@VV@VV@VE�@VE�@V$�@U�@U�-@U�h@Up�@U`B@U?}@U?}@U?}@U?}@U/@U�@T��@T��@T�@TZ@T�@S�
@S�F@S�@S"�@R�@Q�^@Q��@Qhs@QX@Q7L@Q%@P�`@P�9@Pr�@PbN@PA�@P1'@P  @O�@O�w@OK�@O+@O�@O
=@N�@M@MV@L�j@Lz�@K�m@KS�@J��@J��@J~�@I��@I��@I�^@I��@I��@I�7@Ihs@IX@HĜ@H1'@Hb@H  @H  @H  @H  @G��@Gl�@F��@Fȴ@Fff@FV@FV@Fff@FV@Fff@E�@E@E��@E�h@E�@E`B@EV@D�/@D�D@D�@C��@C�
@C�F@CC�@B��@Ahs@@Q�@?�@?�w@>�y@>ȴ@>�+@=��@=�@<�@<��@<I�@<9X@;��@;�F@;"�@:^5@:-@9�^@9&�@8�@7�w@7l�@7+@6�@6v�@6V@6E�@6$�@5�@5��@5�-@5��@5�@5p�@5`B@5`B@5O�@5?}@5/@4��@4�/@4��@4�j@4�@4�D@4��@4��@4��@4j@4(�@4�@41@3�m@3ƨ@3ƨ@3�F@3��@3��@3��@3t�@3dZ@3dZ@3S�@3dZ@3dZ@3S�@3S�@3S�@333@3o@3o@3@2�@2�H@2��@2��@2��@2�!@2~�@2J@1�#@1%@0bN@/�@.{@-@-?}@,��@,�/@,�j@,��@,j@,(�@+��@+�m@+ƨ@+��@+t�@+dZ@+dZ@+33@*�@*n�@*^5@*M�@*=q@*-@*�@*�@*J@)��@)�^@)��@)x�@)G�@)%@(��@(��@(Ĝ@(�@(bN@(bN@(Q�@(A�@( �@'�;@'\)@&�@&E�@%�T@%p�@$��@$��@$�D@$�D@$9X@#dZ@"��@"n�@"=q@!��@!�^@!7L@ �9@ A�@   @��@�@l�@�y@5?@@��@p�@?}@��@��@�j@�@�@�@�@��@�D@j@Z@9X@��@�F@�@dZ@S�@C�@"�@o@�@��@=q@�#@��@x�@G�@�@  @�P@l�@;d@�@�y@ȴ@�R@�R@��@��@�+@ff@V@{@��@�@?}@��@I�@dZ@�@�!@~�@M�@J@��@��@�#@��@��@��@��@hs@7L@�@%@bN@1'@b@�@��@�P@�y@v�@E�@$�@{@�@�T@��@�-@�-@��@`B@`B@?}@?}@/@V@��@��@��@�/@��@�/@�/@�@z�@Z@I�@�@1@�m@�F@��@@
�\@
~�@
~�@
�\@
�\@
�\@
~�@
M�@
-@	��@	x�@	&�@��@Ĝ@Q�@ �@�;@��@�w@�@�P@|�@|�@l�@l�@l�@l�@l�@\)@\)@\)@K�@��@��@�y@�@ȴ@ȴ@�+@�@�T@��@@@��@�h@`B@/@/@V@��@��@�@�@�j@j@I�@9X@�@�@1@��@�m@�m@ƨ@��@�@C�@�H@��@^5@M�@=q@-@J@��@��@��@��@�@�^@ �`@ ��@ �u@ A�?��;?�|�?��?�V?�{?���?��h?�p�?�p�?�O�?�O�?�/?�/?�V11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111119A�S�A�XA�XA�VA�S�A�O�A�"�A�VA���A���A���A��A��`A���AļjAĕ�A�hsA�E�A�1'A��A�A��A��mA�K�A�A\A�v�A��A�+A�r�A���A��FA��9A��A��A�r�A��yA�bNA�O�A�1'A�Q�A�hsA�t�A��A���A�ZA�bNA�S�A�Q�A�ffA�VA�ĜA��`A�n�A��hA��A�I�A�S�A��A���A�A���A�5?A�Q�A�hsA�hsA��A�S�A�?}A�\)A���A��A�O�A�(�A�
=A���A� �A��9A�p�A��`A�A�K�A�&�A�1A"�A~�!A~�+A~9XA}�A}
=A|5?A{\)AzbNAx�DAt�At�+AtbNAt1AshsAs
=Aq;dApn�Am��Ag"�AfZAf1'Ae|�Ad�AdJAc��Ac�Ad�Ad1'Ad=qAct�Ac�AbĜAb�Aa&�A_��A^I�A]x�A]G�A\�uAZjAY��AX��AX��AX=qAW��AV��ATȴAS�PASdZAQ�AP��AO�wAM��AL~�AJȴAIt�AH9XAG�7AG�AF��AE�wAD�yAD �ACx�ABn�AA�PAA�A@��A@��A@bNA?�-A>�!A<=qA:Q�A9�mA8�`A8M�A7�mA7A7�A6�uA5XA41'A3�^A2�yA0�A/��A.jA,��A+�FA)��A)|�A)VA(�!A'�PA'XA'&�A&��A&��A&n�A&(�A$5?A#�A#�A#�;A#�hA"�A"A�A!��A!C�A!VA A�A��AA�A��Ar�A9XAbA��A�A�-AoAM�A�;AdZA�TA�Az�A�TA&�A�AXA&�A�PA�9A�DAI�AJA�
A"�A�yAjA�A	/AC�A?}A��An�AJA?}A��A�AI�A��Ax�A �@���@�G�@��9@�I�@�~�@�(�@�O�@�w@��@��D@�(�@�dZ@�!@�@�p�@��`@�bN@�1@��@���@�t�@�o@ꗍ@�~�@�E�@��@陚@�@�O�@��@��/@�Q�@�|�@�9@�-@ܬ@���@�l�@�"�@�O�@�n�@�@Ցh@Ձ@Ձ@Ձ@��@җ�@�/@���@�@���@�Z@�
=@��@���@�S�@�5?@�Q�@þw@�\)@��@�5?@��@��m@�C�@��y@��#@���@�=q@��9@�S�@�v�@�7L@�r�@�  @�l�@��@��@�~�@��@��-@�`B@�/@� �@�$�@�5?@�X@��@�j@�A�@���@��F@���@�S�@�~�@�-@�J@��@��h@�?}@�V@�%@�%@��@��j@�Q�@���@�t�@�~�@���@�+@��+@���@�K�@��@�
=@��y@���@�v�@���@�G�@��`@�n�@��@���@�bN@�bN@�Z@�Q�@�Z@�Z@�A�@�(�@�(�@��m@���@���@��-@���@��@��@��F@���@�\)@�;d@�"�@�@���@�~�@�@�?}@��D@�j@�A�@�1'@��@���@���@��P@�;d@��H@���@���@��\@�V@���@�I�@�
=@��!@��+@�n�@�V@�-@��@�@���@��@�G�@��@��@��u@��@�A�@���@��R@�M�@��h@�j@�b@|�@+@~�R@~�+@}@}/@{��@z�@y��@y��@yx�@xĜ@xbN@xQ�@xb@w�@w
=@v5?@t�@t�D@s��@r��@r^5@rJ@qG�@p�`@p�u@pQ�@p �@o�@o��@o�@o��@o|�@ol�@oK�@o;d@o;d@o+@o+@o�@o
=@n�+@nff@n5?@n@m��@m��@m�-@m��@m��@m�h@m�@mp�@m�@l�@l�j@lz�@lZ@lZ@lI�@l9X@l(�@l�@l�@kƨ@kt�@k"�@jM�@i�^@i�7@i7L@hĜ@f�y@e�@d(�@c�F@cC�@b�H@b~�@bJ@a��@ax�@a&�@`��@`r�@` �@_�@_;d@^�R@^5?@]�@]/@\��@[��@Z��@Z=q@ZJ@Y�@Y�^@Y�7@Yhs@Y7L@Y%@X��@X��@X�u@XbN@X �@W�;@Wl�@V�@Vv�@Vv�@Vv�@Vff@VV@VV@VE�@VE�@V$�@U�@U�-@U�h@Up�@U`B@U?}@U?}@U?}@U?}@U/@U�@T��@T��@T�@TZ@T�@S�
@S�F@S�@S"�@R�@Q�^@Q��@Qhs@QX@Q7L@Q%@P�`@P�9@Pr�@PbN@PA�@P1'@P  @O�@O�w@OK�@O+@O�@O
=@N�@M@MV@L�j@Lz�@K�m@KS�@J��@J��@J~�@I��@I��@I�^@I��@I��@I�7@Ihs@IX@HĜ@H1'@Hb@H  @H  @H  @H  @G��@Gl�@F��@Fȴ@Fff@FV@FV@Fff@FV@Fff@E�@E@E��@E�h@E�@E`B@EV@D�/@D�D@D�@C��@C�
@C�F@CC�@B��@Ahs@@Q�@?�@?�w@>�y@>ȴ@>�+@=��@=�@<�@<��@<I�@<9X@;��@;�F@;"�@:^5@:-@9�^@9&�@8�@7�w@7l�@7+@6�@6v�@6V@6E�@6$�@5�@5��@5�-@5��@5�@5p�@5`B@5`B@5O�@5?}@5/@4��@4�/@4��@4�j@4�@4�D@4��@4��@4��@4j@4(�@4�@41@3�m@3ƨ@3ƨ@3�F@3��@3��@3��@3t�@3dZ@3dZ@3S�@3dZ@3dZ@3S�@3S�@3S�@333@3o@3o@3@2�@2�H@2��@2��@2��@2�!@2~�@2J@1�#@1%@0bN@/�@.{@-@-?}@,��@,�/@,�j@,��@,j@,(�@+��@+�m@+ƨ@+��@+t�@+dZ@+dZ@+33@*�@*n�@*^5@*M�@*=q@*-@*�@*�@*J@)��@)�^@)��@)x�@)G�@)%@(��@(��@(Ĝ@(�@(bN@(bN@(Q�@(A�@( �@'�;@'\)@&�@&E�@%�T@%p�@$��@$��@$�D@$�D@$9X@#dZ@"��@"n�@"=q@!��@!�^@!7L@ �9@ A�@   @��@�@l�@�y@5?@@��@p�@?}@��@��@�j@�@�@�@�@��@�D@j@Z@9X@��@�F@�@dZ@S�@C�@"�@o@�@��@=q@�#@��@x�@G�@�@  @�P@l�@;d@�@�y@ȴ@�R@�R@��@��@�+@ff@V@{@��@�@?}@��@I�@dZ@�@�!@~�@M�@J@��@��@�#@��@��@��@��@hs@7L@�@%@bN@1'@b@�@��@�P@�y@v�@E�@$�@{@�@�T@��@�-@�-@��@`B@`B@?}@?}@/@V@��@��@��@�/@��@�/@�/@�@z�@Z@I�@�@1@�m@�F@��@@
�\@
~�@
~�@
�\@
�\@
�\@
~�@
M�@
-@	��@	x�@	&�@��@Ĝ@Q�@ �@�;@��@�w@�@�P@|�@|�@l�@l�@l�@l�@l�@\)@\)@\)@K�@��@��@�y@�@ȴ@ȴ@�+@�@�T@��@@@��@�h@`B@/@/@V@��@��@�@�@�j@j@I�@9X@�@�@1@��@�m@�m@ƨ@��@�@C�@�H@��@^5@M�@=q@-@J@��@��@��@��@�@�^@ �`@ ��@ �u@ A�?��;?�|�?��?�V?�{?���?��h?�p�?�p�?�O�?�O�?�/?�/?�V1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111LB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��BBB1B	7B
=B
=B
=B	7B%B  B��B��B��B�B�B�BB��B�LB��B��B��B�7Bo�B]/B6FB �B{BPB1BBB��B��B��B�B�5B�B��B�}B��B�bBz�Br�Bq�Br�Bq�Bo�BbNBXBM�BC�B?}B5?B�B{BPB
=B1B%BB
��B
��B
�B
�B
�TB
��B
�}B
�B
��B
��B
��B
��B
�{B
�\B
�DB
�B
~�B
u�B
gmB
N�B
K�B
I�B
F�B
B�B
>wB
2-B
+B
�B	�B	�sB	�fB	�NB	�/B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	ȴB	�wB	�9B	�B	��B	��B	��B	�hB	�VB	�\B	�PB	�PB	�7B	{�B	r�B	o�B	e`B	_;B	XB	K�B	A�B	6FB	/B	)�B	%�B	#�B	!�B	�B	�B	{B	bB	JB	1B	+B	B	B	B��B��B�B�fB�TB�;B�/B�#B�B�B��B��B��BȴBĜB�wB�XB�9B�!B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�oB�hB�bB�bB�VB�VB�PB�PB�JB�DB�7B�+B�B�B~�B{�By�Bv�Bq�Bn�Bk�Be`BbNBaHB`BB`BB_;B^5B\)B[#BYBVBQ�BO�BP�BO�BM�BL�BL�BK�BI�BI�BH�BF�BE�BB�BB�BB�BA�B@�B>wB=qB;dB9XB8RB7LB7LB7LB6FB5?B49B49B33B33B49B33B33B33B33B33B33B33B33B49B33B33B33B0!B-B+B+B+B+B+B(�B'�B'�B'�B'�B'�B'�B%�B&�B&�B&�B&�B&�B'�B'�B)�B+B+B+B-B-B-B-B-B.B.B/B/B0!B33B5?B6FB6FB7LB8RB9XB9XB:^B;dB;dB<jB=qB>wB?}B>wB@�BE�BB�BP�BR�BS�BS�BT�BVBVBVBYBYBZBZB[#B[#B\)B[#B[#B\)B\)B]/B_;BaHBdZBk�Bp�Bq�By�B�B�B�B�B�B�B�+B�7B�=B�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�'B�-B�3B�3B�9B�9B�?B�?B�FB�LB�^B��BŢBǮBɺBɺB��B��B��B��B��B�B�
B�
B�B�B�#B�fB�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B	B		7B	JB	oB	�B	�B	 �B	!�B	#�B	$�B	'�B	)�B	1'B	8RB	9XB	:^B	;dB	?}B	A�B	A�B	C�B	E�B	H�B	L�B	Q�B	R�B	W
B	\)B	]/B	_;B	bNB	dZB	ffB	gmB	hsB	iyB	jB	k�B	k�B	l�B	l�B	m�B	m�B	n�B	n�B	n�B	n�B	n�B	q�B	r�B	s�B	t�B	u�B	u�B	v�B	v�B	v�B	w�B	w�B	w�B	y�B	z�B	{�B	}�B	}�B	~�B	~�B	� B	� B	� B	� B	�B	�B	�B	�7B	�JB	�PB	�\B	�hB	��B	��B	��B	��B	�B	�B	�B	�!B	�'B	�3B	�9B	�?B	�FB	�RB	�XB	�dB	�qB	�}B	��B	B	ĜB	ƨB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�B	�#B	�#B	�#B	�#B	�#B	�)B	�)B	�/B	�/B	�5B	�5B	�5B	�5B	�5B	�5B	�5B	�;B	�;B	�BB	�BB	�HB	�NB	�NB	�TB	�TB	�fB	�mB	�sB	�sB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
+B
+B
1B
	7B
	7B
	7B
	7B

=B
DB
PB
bB
hB
hB
oB
uB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
!�B
!�B
"�B
#�B
$�B
$�B
$�B
$�B
$�B
$�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
&�B
&�B
&�B
&�B
&�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
(�B
(�B
(�B
(�B
(�B
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
)�B
)�B
+B
+B
+B
+B
+B
+B
,B
,B
-B
.B
0!B
2-B
2-B
33B
33B
33B
49B
49B
49B
49B
5?B
5?B
5?B
5?B
6FB
6FB
6FB
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
9XB
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
<jB
=qB
>wB
>wB
?}B
@�B
@�B
A�B
A�B
A�B
B�B
C�B
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
H�B
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
M�B
M�B
M�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
O�B
P�B
Q�B
Q�B
Q�B
Q�B
R�B
S�B
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
VB
VB
VB
W
B
W
B
XB
XB
YB
YB
ZB
[#B
[#B
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
^5B
^5B
^5B
_;B
_;B
_;B
_;B
_;B
_;B
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
dZB
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
hsB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
iyB
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
m�B
m�B
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
q�B
q�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
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
v�B
v�B
v�B
v�B
w�B
w�B
w�B
x�B
x�B
x�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111LB��B��B��B�B�B�6B�B��B��B��B��B��B�	B�B�VBoBmBfB	lB
rB
�B
�B
�BEB iB��B��B��B��B�CB�,B�B�6B�fB�'B�IB��Bv+Bf�B<6B$�BmBpB	BYBgB �B��B�B�}B�B��B�&BŢB�yB��B|�Bs�BrBsBr�Br�BdtBZQBO�BE9BA�B9XB �B�B�B
�B�B�BB
�B
��B
�B
�IB
�2B
�SB
�'B
�[B
��B
�#B
��B
�
B
�MB
�B
�dB
�YB
��B
xRB
j�B
OvB
LB
JXB
GzB
C�B
@�B
49B
/ B
�B	�B	��B	�RB	�:B	��B	�KB	��B	��B	�B	�KB	�B	ևB	ԕB	��B	�BB	�rB	� B	�%B	��B	�QB	��B	��B	�:B	��B	��B	�VB	��B	�xB	}VB	s�B	q[B	f�B	aB	Z�B	M�B	C�B	8B	0�B	*�B	&�B	$�B	# B	�B	�B	�B	�B	PB	�B	�B	�B	�B	AB��B��B�B�8B�B�B��BیB��B�eB՛B�HB��B�=B��B�B�JB�FB��B��B��B��B��B�B�B�B�-B�TB�ZB��B��B�!B��B�B�pB��B�IB�]B�QB�1B��B�MB�&B��B��B��B��B��B��B��B��B�0B�=B��B�?B�B�B}B{By�Br�Bp�Bn/BgRBcTBa�B`�B`�B_�B_!B\�B\)BZ�BYeBS�BP.BQ�BP�BN�BN<BN�BL�BJ�BJXBI�BG�BG+BC�BC-BCGBB�BBAB@OB>�B<�B:DB8�B7�B7�B7�B6�B5�B4�B4�B3hB3hB4�B3�B3�B3hB3�B3�B3�B3hB3hB4�B3�B3�B4nB2�B0�B,B+�B+kB+�B,�B*�B(sB($B(
B($B(>B(�B'mB'�B'�B'�B'�B(
B)*B)DB*�B+�B+�B,"B-�B-wB-�B-�B-�B.�B.�B/�B0!B1�B4TB6`B7LB7B88B8�B9�B9�B:�B;�B;�B<�B=�B>�B?�B?}BA�BFtBESBQNBS&BT,BTFBUMBVSBVmBV�BYeBYKBZQBZkB[�B[WB\CB[=B[WB\xB\�B]�B_�BbNBe�Bl�Bq[Br�Bz�B�;B�;B�AB�AB�uB��B��B�	B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�pB��B��B��B�[B�aB�hB��B�nB�nB�tB��B��B��B��B��BżB��B��B�	B�B�B�B�HB�:B�B�?B�?B�_BٴB�CB�B��B��B��B��B��B��B�B��B��B��B�B�B��B�B�PB��B	�B		�B	B	&B	�B	B	 �B	!�B	$B	%,B	(XB	*�B	1�B	8lB	9rB	:�B	;�B	?�B	A�B	A�B	C�B	E�B	IB	M6B	R:B	S[B	WYB	\]B	]dB	_�B	b�B	d�B	f�B	g�B	h�B	i�B	j�B	k�B	k�B	l�B	l�B	m�B	m�B	n�B	n�B	n�B	n�B	n�B	q�B	r�B	s�B	t�B	u�B	u�B	v�B	v�B	v�B	w�B	w�B	w�B	y�B	z�B	|B	~B	~B	B	.B	�B	�B	�B	�4B	�AB	�gB	��B	��B	�dB	��B	��B	� B	�B	�-B	�B	�*B	�QB	�CB	�IB	�;B	�[B	�MB	�nB	�tB	�zB	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B	�B	� B	�B	� B	�B	�B	�B	�,B	�B	�9B	�?B	�KB	�B	�B	�B	�7B	�=B	�=B	�#B	�=B	�=B	�CB	�CB	�IB	�IB	�OB	�5B	�5B	�5B	�OB	�jB	�OB	�pB	�pB	�\B	�\B	�bB	�hB	�hB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	�B	��B	�B	�B	�B	��B	�B	��B	��B	��B	��B	��B	�B	��B	�B	�*B	��B	��B	��B	��B	�B	�B	�B	�B	�(B	�B	�B
  B
  B
 4B
 B
 4B
;B
AB
-B
-B
B
B
-B
B
GB
3B
9B
SB
9B
?B
?B
EB
_B
fB
	RB
	RB
	RB
	lB

�B
�B
�B
}B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
B
 �B
!�B
!�B
"�B
#�B
$�B
$�B
$�B
$�B
$�B
$�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
'B
&�B
&�B
&�B
'B
'�B
'�B
(
B
(
B
(
B
'�B
(
B
)B
)B
)B
(�B
(�B
(�B
(�B
)B
(�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
*B
*B
)�B
)�B
)�B
*0B
+B
+B
+B
+B
+6B
+QB
,=B
,WB
-]B
.�B
0oB
2GB
2aB
3MB
3MB
3MB
4TB
4TB
4TB
4TB
5ZB
5ZB
5ZB
5ZB
6FB
6FB
6`B
6`B
6�B
7LB
7LB
7fB
8RB
8lB
8RB
8RB
8lB
8�B
8lB
9rB
9rB
9rB
:^B
:xB
:xB
:xB
;dB
;B
;dB
;B
;B
;B
;�B
<�B
=�B
>�B
>�B
?�B
@�B
@�B
A�B
A�B
A�B
B�B
C�B
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
H�B
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
M�B
M�B
NB
N�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
PB
Q B
RB
RB
RB
R:B
S&B
T,B
UB
U2B
UB
UB
VB
VB
VB
VB
VB
VB
V9B
VB
VB
W?B
W?B
XEB
XEB
YKB
YB
ZQB
[=B
[=B
\CB
\CB
\)B
]/B
]IB
]/B
]/B
]IB
]IB
]IB
]IB
^OB
^OB
^jB
_VB
_pB
_VB
_VB
_VB
_pB
abB
a|B
bhB
bhB
bhB
bNB
bNB
cTB
cnB
cnB
cnB
cnB
cnB
dtB
dZB
dtB
dZB
dZB
dtB
dtB
dZB
dZB
dZB
dtB
ezB
ezB
ezB
ezB
ffB
f�B
f�B
f�B
f�B
g�B
h�B
hsB
hsB
hsB
hsB
h�B
h�B
h�B
h�B
h�B
i�B
j�B
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
m�B
m�B
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
q�B
q�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
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
uB
v�B
v�B
v�B
v�B
w�B
w�B
w�B
x�B
x�B
x�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111�@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.02(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201711210033452017112100334520171121003345202211182132302022111821323020221118213230201804031938032018040319380320180403193803  JA  ARFMdecpA19c                                                                20171110003506  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20171109153600  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20171109153602  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20171109153602  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20171109153603  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20171109153603  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20171109153603  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20171109153603  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20171109153603  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20171109153603                      G�O�G�O�G�O�                JA  ARUP                                                                        20171109155631                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20171109153339  CV  JULD            G�O�G�O�F��                JM  ARCAJMQC2.0                                                                 20171120153345  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20171120153345  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180403103803  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200114171536                      G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20221118123230  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20221123111507                      G�O�G�O�G�O�                