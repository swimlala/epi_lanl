CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-06-19T17:08:59Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         ZPRIMARY | https://orcid.org/0000-0001-7324-3159 | Matthew Alkire, University of Washington        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7(   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7,   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    70   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7@   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7P   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7`   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7h   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    8   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    8    DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     8$   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8D   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8H   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8L   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8l   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8�   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8�   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           8�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    g�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  i�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  q�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    y�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �T   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �d   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �h   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �x   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �|   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200619170859  20220204114413  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  7662                            2C  D   APEX                            8312                            080318                          846 @؍���1   @؍F8@8CS����cڗ�O�;1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   B   B   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A���A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`ffBh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CE�fCH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D�fDfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7fD7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DSy�DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�fDy�{D��D�Z�D���D��\D�D�MqD���D���D�)D�_�D��D���D��D�G\Dڬ)D��D��D�W
D��D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @���@�\)@�\)A�A?�A_�A�A��
A��
A��
A��
A��
A��A��
A��
B�B�B�B�B'�B/�B7�B?�BG�BO�BW�B`Q�Bg�Bo�Bw�B�B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE�GCG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC�
>C��qC��qC��C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC�
>C��qC��qC��qC��qC��qC��qC�
>C�
>C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qD ~�D ��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D	~�D	��D
~�D
��D~�D��D~�D��D~�D��D~�D��D~�D�RD~�D��D~�D��D~�D��D~�D��D�DD~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D ~�D ��D!~�D!��D"~�D"��D#~�D#��D$~�D$��D%~�D%��D&~�D&��D'~�D'��D(~�D(��D)~�D)��D*~�D*��D+~�D+��D,~�D,��D-~�D-��D.~�D.��D/~�D/��D0~�D0��D1~�D1��D2~�D2��D3~�D3��D4~�D4��D5~�D5��D6~�D7D7~�D7��D8~�D8��D9~�D9��D:~�D:��D;~�D;��D<~�D<��D=~�D=��D>~�D>��D?~�D?��D@~�D@��DA~�DA��DB~�DB��DC~�DC��DD~�DD��DE~�DE��DF~�DF��DG~�DG��DH~�DH��DI~�DI��DJ~�DJ��DK~�DK��DL~�DL��DM~�DM��DN~�DN��DO~�DO��DP~�DP��DQ~�DQ��DR~�DR��DSxRDS��DT~�DT��DU~�DU��DV~�DV��DW~�DW��DX~�DX��DY~�DY��DZ~�DZ��D[~�D[��D\~�D\��D]~�D]��D^~�D^��D_~�D_��D`~�D`��Da~�Da��Db~�Db��Dc~�Dc��Dd~�Dd��De~�De��Df~�Df��Dg~�Dg��Dh~�Dh��Di~�Di��Dj~�Dj��Dk~�Dk��Dl~�Dl��Dm~�Dm��Dn~�Dn��Do~�Do��Dp~�Dp��Dq~�Dq��Dr~�Dr��Ds~�Ds��Dt~�Dt�Dy�3D�D�Y�D��)D�θD�pD�L�D��3D��HD��D�_
D��{D��3D�D�F�Dګ�D��pD�=D�VfD�)D��3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AҸRAҮAҴ9AҶFAҸRAҺ^AҼjAҺ^AҺ^AҸRAҴ9AҮAҋDA�z�A�ffA�dZA�jA�p�A҅Aң�AҲ-AҶFAҾwA�ƨA�ȴAҼjA�~�A�oA��A��A�9XA��mA��A�VA�;dA�l�A�A§�A�9XA��A��^A���A�&�A���A�|�A��^A�\)A�S�A�z�A��;A��A�JA���A���A�Q�A���A���A��HA�&�A��A�ĜA�v�A�VA�E�A�  A��A��mA���A�hsA���A�VA��wA�  A���A���A�(�A��`A�%A�ĜA�JA��A���A��uA�&�A��
A�C�A��;A��A���A��A���A�ffA���A�|�A���A��^A�G�A�K�A��;A�l�A�+A��\A��A�JA�JA��A�%A��A}�^Azn�Av�Au��At�+ArȴAq�7AmXAjjAioAhffAe33Ab��AbVAa�hA`1'A\�AZ��AYt�AW��AUt�ASp�ARz�AQ�mAQ�AO&�AM7LAJffAH�RAG��AG/AF-AD��ACt�AB�`AA�wA@�HA@��A?�A=�A:$�A8�+A7hsA6ĜA5��A4=qA3�7A2�`A1�mA1x�A1hsA1VA0��A/�A.��A.�+A-oA+7LA*1A);dA(�uA(bNA({A'G�A&��A%��A$�RA#�A#33A"=qA!��A ��A ZA��Ar�AA��AVA��A�Ax�A�A��A�AȴA�A��A|�AhsAĜAƨA�!AhsA��A��Av�AVA$�AdZA�jA��A�wA��AdZA�A��A7LA
�9A	`BA9XA\)A1'A"�A��A~�A9XA�
A?}AA�A �RA {@��@�b@�"�@�`B@�\)@�{@���@���@���@���@�+@���@�b@�F@�\)@�"�@��H@�7@畁@��y@��@�K�@�-@��@���@���@�(�@�
=@�@�Ĝ@׮@�~�@�7L@��
@��@���@�@�V@�r�@�S�@�E�@�X@��@�j@���@�\)@�@�X@�1@�@�/@��D@�(�@��\@��T@��@�r�@��@��P@�ȴ@��\@�E�@���@�hs@��@��/@�j@��m@���@�S�@���@�^5@�@���@�p�@�/@�V@��/@���@��@���@��F@���@�t�@��@���@���@���@�^5@��@�@���@�x�@�%@��@��@�;d@��H@��\@�E�@�$�@��@���@�hs@��@���@���@��/@���@��u@�1'@�\)@���@��@�hs@�hs@��@�n�@��@���@��@���@�\)@��`@��
@�
=@�ff@�9X@�V@���@��-@�J@���@��\@��@�O�@��@��
@���@���@���@�\)@�o@���@�-@�@�@���@�G�@�/@��@�&�@�x�@��@�z�@�Z@�Q�@� �@��@�b@��@��!@��+@�n�@�ff@�E�@��T@�G�@�V@��@�Ĝ@��9@�I�@�t�@��@�@��H@��R@���@��\@��+@�~�@�n�@�5?@�J@���@�X@��/@���@��u@��9@�Ĝ@���@��D@��u@�Z@�1'@���@�C�@��R@�K�@��@�+@��R@�J@���@�@�&�@��D@�9X@���@�C�@�@���@��R@�n�@�v�@�v�@�n�@�ff@�ff@�^5@�=q@��T@��-@���@�p�@��@���@���@��j@���@�j@�1'@��m@���@��P@�K�@��@��@�@��y@��!@��\@�5?@��#@���@��-@���@���@��7@�`B@�O�@�?}@�/@��@���@��/@��j@��u@�2�@z��@q�@htT@a�7@X�@R�@L!@Dw�@<w�@5e,@0@)��@$�?@!�@ϫ@$t@��@v`@x@
�L111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  AҸRAҮAҴ9AҶFAҸRAҺ^AҼjAҺ^AҺ^AҸRAҴ9AҮAҋDA�z�A�ffA�dZA�jA�p�A҅Aң�AҲ-AҶFAҾwA�ƨA�ȴAҼjA�~�A�oA��A��A�9XA��mA��A�VA�;dA�l�A�A§�A�9XA��A��^A���A�&�A���A�|�A��^A�\)A�S�A�z�A��;A��A�JA���A���A�Q�A���A���A��HA�&�A��A�ĜA�v�A�VA�E�A�  A��A��mA���A�hsA���A�VA��wA�  A���A���A�(�A��`A�%A�ĜA�JA��A���A��uA�&�A��
A�C�A��;A��A���A��A���A�ffA���A�|�A���A��^A�G�A�K�A��;A�l�A�+A��\A��A�JA�JA��A�%A��A}�^Azn�Av�Au��At�+ArȴAq�7AmXAjjAioAhffAe33Ab��AbVAa�hA`1'A\�AZ��AYt�AW��AUt�ASp�ARz�AQ�mAQ�AO&�AM7LAJffAH�RAG��AG/AF-AD��ACt�AB�`AA�wA@�HA@��A?�A=�A:$�A8�+A7hsA6ĜA5��A4=qA3�7A2�`A1�mA1x�A1hsA1VA0��A/�A.��A.�+A-oA+7LA*1A);dA(�uA(bNA({A'G�A&��A%��A$�RA#�A#33A"=qA!��A ��A ZA��Ar�AA��AVA��A�Ax�A�A��A�AȴA�A��A|�AhsAĜAƨA�!AhsA��A��Av�AVA$�AdZA�jA��A�wA��AdZA�A��A7LA
�9A	`BA9XA\)A1'A"�A��A~�A9XA�
A?}AA�A �RA {@��@�b@�"�@�`B@�\)@�{@���@���@���@���@�+@���@�b@�F@�\)@�"�@��H@�7@畁@��y@��@�K�@�-@��@���@���@�(�@�
=@�@�Ĝ@׮@�~�@�7L@��
@��@���@�@�V@�r�@�S�@�E�@�X@��@�j@���@�\)@�@�X@�1@�@�/@��D@�(�@��\@��T@��@�r�@��@��P@�ȴ@��\@�E�@���@�hs@��@��/@�j@��m@���@�S�@���@�^5@�@���@�p�@�/@�V@��/@���@��@���@��F@���@�t�@��@���@���@���@�^5@��@�@���@�x�@�%@��@��@�;d@��H@��\@�E�@�$�@��@���@�hs@��@���@���@��/@���@��u@�1'@�\)@���@��@�hs@�hs@��@�n�@��@���@��@���@�\)@��`@��
@�
=@�ff@�9X@�V@���@��-@�J@���@��\@��@�O�@��@��
@���@���@���@�\)@�o@���@�-@�@�@���@�G�@�/@��@�&�@�x�@��@�z�@�Z@�Q�@� �@��@�b@��@��!@��+@�n�@�ff@�E�@��T@�G�@�V@��@�Ĝ@��9@�I�@�t�@��@�@��H@��R@���@��\@��+@�~�@�n�@�5?@�J@���@�X@��/@���@��u@��9@�Ĝ@���@��D@��u@�Z@�1'@���@�C�@��R@�K�@��@�+@��R@�J@���@�@�&�@��D@�9X@���@�C�@�@���@��R@�n�@�v�@�v�@�n�@�ff@�ff@�^5@�=q@��T@��-@���@�p�@��@���@���@��j@���@�j@�1'@��m@���@��P@�K�@��@��@�@��y@��!@��\@�5?@��#@���@��-@���@���@��7@�`B@�O�@�?}@�/@��@���@��/@��jG�O�@�2�@z��@q�@htT@a�7@X�@R�@L!@Dw�@<w�@5e,@0@)��@$�?@!�@ϫ@$t@��@v`@x@
�L111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B��B��B��B%BoB�B�B"�B&�B1'B:^BI�BN�BM�BL�BE�BK�BJ�BL�BYBXB`BB`BBq�Bu�B�B�=B�+B�%B�B�VB�bB��B�hB�uB��B��B��B��B�uB�hB�VB�VB�PB�7B�DB� B�Bz�B|�B{�Bz�By�Bw�Bt�Bk�BffB^5B[#BVBH�B9XB'�B&�B�B{BB��B��B�dB�LB��B�hB�=B~�BgmBcTBXBH�BD�B>wB8RB)�B!�B�BB
��B
�B
��B
�}B
��B
|�B
cTB
=qB
�B	��B	�TB	�B	ɺB	�qB	��B	}�B	r�B	jB	_;B	Q�B	M�B	I�B	D�B	+B	�B	uB	+B��B�B�TB�;B�B��BĜB�RB�'B�B��B��B��B��B��B��B��B�uB�VB�Bv�Bp�Bp�Bm�Bm�Bn�Bn�Bu�Bu�B|�B~�B� B~�B}�Bz�B{�B{�Bv�Br�Bp�Bn�Bm�Bm�Bl�Bl�BjBiyBiyBhsBdZB_;B]/B[#BZBXBW
BW
BXBVBT�BS�BQ�BP�BN�BL�BI�BH�BG�BF�BE�BB�B@�B=qB<jB<jB;dB;dB9XB9XB8RB7LB6FB5?B49B5?B2-B33B33B49B1'B/B0!B-B-B-B,B+B)�B(�B$�B$�B%�B#�B"�B#�B#�B"�B#�B#�B%�B,B/B0!B33B2-B33B33B33B5?B7LB7LB:^B:^B;dB>wBA�B@�B>wB=qB?}BC�BI�BO�BQ�BW
BXB]/B]/B]/B_;B_;B`BBaHBbNBbNBe`BhsBm�Bl�Bl�Bn�Br�Br�Br�Bt�Bu�Bz�B}�B� B�B�B�B�+B�7B�DB�PB�VB�bB�uB�{B��B��B��B��B��B��B��B��B�B�B�B�?B�FB�RB�XB�^B�jB��BÖBŢBɺB��B��B��B��B��B��B�B�)B�;B�HB�NB�`B�sB�B�B�B�B�B��B��B��B��B��B	B	B	B	B	B��B��B	B	%B	DB		7B	B	B	B��B��B��B��B��B��B	B	%B	%B	
=B	1B	1B		7B	
=B	DB	DB	VB	bB	hB	oB	{B	�B	�B	�B	�B	 �B	%�B	)�B	,B	,B	+B	+B	,B	2-B	49B	5?B	7LB	8RB	:^B	?}B	G�B	I�B	J�B	L�B	L�B	P�B	ZB	]/B	^5B	_;B	aHB	e`B	ffB	gmB	gmB	k�B	n�B	q�B	u�B	x�B	{�B	�B	�%B	�7B	�\B	�bB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�-B	�3B	�9B	�?B	�FB	�^B	�}B	��B	B	ÖB	ŢB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�B	�B	�/B	�5B	�;B	�BB	�HB	�NB	�NB	�NB	�NB	�NB	�NB	�NB	�TB	�TB	�TB	�ZB	�B	�wB
�B
B
+B
#B
*0B
.�B
9	B
C-B
IB
NB
R:B
XB
]/B
a�B
c B
e�B
iyB
o B
r�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�#B�0B�aB	�B�B�BB#B(`B1�B@�BFBE
BDB<�BB�BA�BDBPOBOHBWzBWzBh�Bl�ByBB�sB~bB}\B|VB��B��B��B��B��B��B��B��B��B��B��B��B��B��B�pB�}Bw:Bx@BrBt(Bs!BrBqBo
Bk�Bb�B]�BUrBR`BMBB?�B0�B1B*B�B�B�JB�B�'B��B��B�B��B��BvHB^�BZ�BOaB@B;�B5�B/�B!QB B�B
�jB
�4B
��B
�HB
��B
�0B
tPB
Z�B
4�B
�B	�/B	��B	ыB	�)B	��B	�0B	uiB	j&B	a�B	V�B	IdB	ELB	A3B	<B	"~B	)B	
�B��B�IB�B��BּBЙB�PB� B��B��B��B��B�iB�LB�-B�'B�"B�B��B��B{�BnTBh/Bh/BeBeBf$Bf$BmNBmOBtyBv�Bw�Bv�BuBrmBsrBssBnUBj=Bh1Bf%BeBeBdBdBbBaBaB`B[�BV�BT�BR�BQ�BO�BN�BN�BO�BM�BL�BK�BI|BHvBFjBD^BAKB@EB?@B>:B=4B:!B8B5B3�B3�B2�B2�B0�B0�B/�B.�B-�B,�B+�B,�B)�B*�B*�B+�B(�B&�B'�B$�B$�B$�B#�B"�B!�B �BsBsByBnBhBnBnBhBnBnBzB#�B&�B'�B*�B)�B*�B*�B*�B,�B.�B.�B1�B1�B2�B6B9B8B6B5B7B;-BAPBGuBI�BN�BO�BT�BT�BT�BV�BV�BW�BX�BY�BY�B\�B`Be&Bd Bd Bf-BjEBjEBjEBlQBmXBrvBu�Bw�By�B|�B|�B~�B��B��B��B��B��B�B�B�B�-B�?B�QB�dB�jB�{B��B��B��B��B��B��B��B��B��B��B�B�'B�3B�JB�]B�cB�iB�|BˈB̎BѬBӸB��B��B��B��B�B�B�B�,B�2B�DB�]B�]B�cB�oB�uB��B��B��B��B��B�|B��B��B��B	�B	 �B��B��B��B��B�RB�RB�RB�XB�uB��B��B��B	�B��B��B	 �B	�B	�B	�B	�B	�B	�B		�B	B	 B	'B	9B	>B	QB	oB	!�B	#�B	#�B	"�B	"�B	#�B	)�B	+�B	,�B	.�B	/�B	1�B	7B	?8B	ADB	BKB	DWB	DWB	HoB	Q�B	T�B	U�B	V�B	X�B	\�B	]�B	^�B	^�B	cB	f B	i2B	mJB	p\B	snB	x�B	}�B	��B	��B	��B	��B	�B	�B	�%B	�B	�B	�B	�7B	�JB	�JB	�=B	�7B	�=B	�JB	�VB	�VB	��B	�zB	�zB	�zB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	�B	�B	�B	�%B	�1B	�7B	�IB	�aB	�nB	�tB	�zB	�zB	̀B	̀B	͆B	͆B	ΌB	ϒB	ЙB	џB	԰B	նB	ּB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��G�O�B	�B	��B
  B

�B
�B
�B
!�B
&JB
0�B
:�B
@�B
E�B
I�B
O�B
T�B
Y.B
Z�B
]FB
`�B
f|B
j,111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.02 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9998(+/-0.0001), vertically averaged dS =-0.008(+/-0.002) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144132022020411441320220204114413  AO  ARCAADJP                                                                    20200619170859    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200619170859  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200619170859  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114413  IP                  G�O�G�O�G�O�                