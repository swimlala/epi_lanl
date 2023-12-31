CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-06-18T14:13:37Z creation      
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200618141337  20220204114411  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  7662                            2C  D   APEX                            8312                            080318                          846 @؂܆B�1   @؂�`@7bM���c�����1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   B   B   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8ffB@ffBH  BP  BX  B`  Bh  Bp  Bx  B�33B�33B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D fD � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP�fDQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt�fDt��Dy��D��D�aHD��fD��D�%qD�^fD���D��)D�(�D�]�D���D��D�3D�UDډHD�� D�*=D�^fD�\D��
111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�(�@�\)@�\)A�A?�A_�A�A��
A��
A��
A��
A��
A��
A��
A��
B�B�B�B�B'�B/�B8Q�B@Q�BG�BO�BW�B_�Bg�Bo�Bw�B�(�B�(�B���B�B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B�B���B���B���B���B���B���B���B���B���B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C�
>C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qD D ~�D ��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D	~�D	��D
~�D
��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D ~�D ��D!~�D!��D"~�D"��D#~�D#��D$~�D$��D%~�D%��D&~�D&��D'~�D'��D(~�D(��D)~�D)��D*~�D*��D+~�D+��D,~�D,��D-~�D-��D.~�D.��D/~�D/��D0~�D0��D1~�D1��D2~�D2��D3~�D3��D4~�D4��D5~�D5��D6~�D6��D7~�D7��D8~�D8��D9~�D9��D:~�D:��D;~�D;��D<~�D<��D=~�D=��D>~�D>��D?~�D?��D@~�D@��DA~�DA��DB~�DB��DC~�DC��DD~�DD��DE~�DE��DF~�DF��DG~�DG��DH~�DH��DI~�DI��DJ~�DJ��DK~�DK��DL~�DL��DM~�DM��DN~�DN��DO~�DO��DP�DP��DQ~�DQ��DR~�DR��DS~�DS��DT~�DT��DU~�DU��DV~�DV��DW~�DW��DX~�DX��DY~�DY��DZ~�DZ��D[~�D[��D\~�D\��D]~�D]��D^~�D^��D_~�D_��D`~�D`��Da~�Da��Db~�Db��Dc~�Dc��Dd~�Dd��De~�De��Df~�Df��Dg~�Dg��Dh~�Dh��Di~�Di��Dj~�Dj��Dk~�Dk��Dl~�Dl��Dm~�Dm��Dn~�Dn��Do~�Do��Dp~�Dp��Dq~�Dq��Dr~�Dr��Ds~�Ds��Dt�Dt˅Dy�fD�)D�`�D���D��pD�$�D�]�D��D�ÅD�( D�]D���D��{D��D�T{Dڈ�D��\D�)�D�]�D�D��f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�VA��A�{A�x�A�n�A�hsA�hsA�t�AӁA�t�A�jA�hsA�hsA�bNA�ZA�\)A�S�A�?}A�A��A��Aҗ�AҋDA� �A��yA��;A�x�A��HA��AǶFA�M�A�33A�/A�  A��PA�~�A�JA�dZA���A���A�p�A�5?A��TA�^5A��+A�jA���A�dZA�?}A���A�-A�~�A�hsA���A���A���A�JA���A���A���A��A�v�A�-A�K�A��-A�?}A�{A��A��uA�%A��
A�S�A���A�?}A�I�A�"�A�XA��A�`BA�5?A��7A�dZA�oA�+A���A�&�A�A�t�A��`A��A�l�A���A�+A�Q�A�+A���A�%A�ĜA�E�A���A��DA�ffA�A�A�A~�HA|�DAx�Awp�AtbNAq
=AnVAl�!AlbAkl�Aj�Aj��Aj-AihsAg��Af�DAe�TAd9XAa��A`�\A`  A_/A^n�A\�jA[�A[�A[&�AYx�AW�PAU�PAQ��AP�RAPn�AO��AN=qAL��AK��AH��AE\)AD�AC��AB�yABffAAt�A@��A?�wA>�/A>$�A<��A<A�A;�A;t�A:�DA9�7A7C�A5�A5�PA5O�A5+A5A4�A2�HA2A�A1�
A0E�A.��A.bA,��A, �A+�A+`BA++A*�!A)�A)�A(1A'\)A&��A%�7A%�A#��A"z�A!\)A �`A E�A��A��AVA�A`BA�^A��A�hA�+A��A�A�mA7LA��A��A��A�+Ar�AC�AO�A��A�AA
-A	�A	�A��A�mAĜA�A�AVA5?A�A�A�#A�7A ��A   @�o@�p�@�C�@�A�@�dZ@��R@��^@�{@�Z@���@��
@��@�
=@�5?@�r�@�
=@��@�@��@�ff@�z�@�5?@�O�@�=q@��`@۶F@�\)@�@�~�@��#@���@�G�@׮@�M�@�9X@�\)@���@��@щ7@�/@��y@�@�G�@�9X@˾w@�+@ʸR@ɲ-@�A�@�  @ǍP@�=q@�p�@��`@ļj@ċD@Ý�@��@�ƨ@��F@��P@�\)@�K�@��!@�x�@���@��@�  @�C�@�-@�O�@�Q�@��w@��@�7L@��j@��D@���@�;d@�n�@���@� �@���@�S�@�
=@�ȴ@���@�Ĝ@���@�@�=q@�@�V@�+@�n�@��@���@���@��@�;d@��y@���@���@���@�j@�j@�r�@�A�@��@��F@���@���@���@���@���@�t�@�S�@�"�@��R@�ff@��@���@�O�@�&�@��@��9@��F@�t�@�S�@�C�@�K�@��@�ȴ@���@���@�{@�O�@�p�@�?}@�?}@��@�%@���@���@���@��@�/@�&�@��@���@�+@�$�@���@��@��/@���@�Ĝ@��D@��@��m@�ƨ@��@�dZ@�K�@�
=@�"�@��@��H@���@��R@���@�ff@��@��^@���@�`B@�7L@���@���@��/@���@�Ĝ@���@��D@�A�@�b@���@�\)@�@���@���@��\@��+@���@�^5@��@���@�`B@�G�@�`B@�`B@�p�@�x�@��@��@��@��/@���@���@��@�j@�I�@��@��
@�\)@�o@�@��y@���@�=q@��@��T@�@��@�?}@��@�&�@�&�@�O�@�7L@���@���@�Ĝ@���@���@��/@���@��@�z�@�1'@�1@��
@��w@��F@��@��P@�S�@�;d@�"�@��@���@��H@���@��R@���@��+@�V@�-@�-@�@���@��@~��@u��@l�@cE9@[A�@S\)@L�@FGE@=x�@7a@2xl@,��@'�@"W�@�@��@K^@�;@�@c�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A�VA��A�{A�x�A�n�A�hsA�hsA�t�AӁA�t�A�jA�hsA�hsA�bNA�ZA�\)A�S�A�?}A�A��A��Aҗ�AҋDA� �A��yA��;A�x�A��HA��AǶFA�M�A�33A�/A�  A��PA�~�A�JA�dZA���A���A�p�A�5?A��TA�^5A��+A�jA���A�dZA�?}A���A�-A�~�A�hsA���A���A���A�JA���A���A���A��A�v�A�-A�K�A��-A�?}A�{A��A��uA�%A��
A�S�A���A�?}A�I�A�"�A�XA��A�`BA�5?A��7A�dZA�oA�+A���A�&�A�A�t�A��`A��A�l�A���A�+A�Q�A�+A���A�%A�ĜA�E�A���A��DA�ffA�A�A�A~�HA|�DAx�Awp�AtbNAq
=AnVAl�!AlbAkl�Aj�Aj��Aj-AihsAg��Af�DAe�TAd9XAa��A`�\A`  A_/A^n�A\�jA[�A[�A[&�AYx�AW�PAU�PAQ��AP�RAPn�AO��AN=qAL��AK��AH��AE\)AD�AC��AB�yABffAAt�A@��A?�wA>�/A>$�A<��A<A�A;�A;t�A:�DA9�7A7C�A5�A5�PA5O�A5+A5A4�A2�HA2A�A1�
A0E�A.��A.bA,��A, �A+�A+`BA++A*�!A)�A)�A(1A'\)A&��A%�7A%�A#��A"z�A!\)A �`A E�A��A��AVA�A`BA�^A��A�hA�+A��A�A�mA7LA��A��A��A�+Ar�AC�AO�A��A�AA
-A	�A	�A��A�mAĜA�A�AVA5?A�A�A�#A�7A ��A   @�o@�p�@�C�@�A�@�dZ@��R@��^@�{@�Z@���@��
@��@�
=@�5?@�r�@�
=@��@�@��@�ff@�z�@�5?@�O�@�=q@��`@۶F@�\)@�@�~�@��#@���@�G�@׮@�M�@�9X@�\)@���@��@щ7@�/@��y@�@�G�@�9X@˾w@�+@ʸR@ɲ-@�A�@�  @ǍP@�=q@�p�@��`@ļj@ċD@Ý�@��@�ƨ@��F@��P@�\)@�K�@��!@�x�@���@��@�  @�C�@�-@�O�@�Q�@��w@��@�7L@��j@��D@���@�;d@�n�@���@� �@���@�S�@�
=@�ȴ@���@�Ĝ@���@�@�=q@�@�V@�+@�n�@��@���@���@��@�;d@��y@���@���@���@�j@�j@�r�@�A�@��@��F@���@���@���@���@���@�t�@�S�@�"�@��R@�ff@��@���@�O�@�&�@��@��9@��F@�t�@�S�@�C�@�K�@��@�ȴ@���@���@�{@�O�@�p�@�?}@�?}@��@�%@���@���@���@��@�/@�&�@��@���@�+@�$�@���@��@��/@���@�Ĝ@��D@��@��m@�ƨ@��@�dZ@�K�@�
=@�"�@��@��H@���@��R@���@�ff@��@��^@���@�`B@�7L@���@���@��/@���@�Ĝ@���@��D@�A�@�b@���@�\)@�@���@���@��\@��+@���@�^5@��@���@�`B@�G�@�`B@�`B@�p�@�x�@��@��@��@��/@���@���@��@�j@�I�@��@��
@�\)@�o@�@��y@���@�=q@��@��T@�@��@�?}@��@�&�@�&�@�O�@�7L@���@���@�Ĝ@���@���@��/@���@��@�z�@�1'@�1@��
@��w@��F@��@��P@�S�@�;d@�"�@��@���@��H@���@��R@���@��+@�V@�-@�-@�G�O�@��@~��@u��@l�@cE9@[A�@S\)@L�@FGE@=x�@7a@2xl@,��@'�@"W�@�@��@K^@�;@�@c�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBq�Bu�B�1B�\B�oB�oB�{B��B��B�9B�XB�^B�^B�}BɺBbBYBe`By�B{�B}�B�B�%B��B��B��B�FB�-B�'B�qB�qB�jB�}B��B��BȴBĜB�wB�XB�'B�-B�9B�?B�FB�!B�B��B��B��B��B��B��B��B�JB�1B� Bz�Bx�Bv�Bu�Br�Bo�BVBE�B?}B=qB:^B9XB7LB#�B�B�B�BoBB�B�ZB�B��B��B�wB��B|�Bq�B\)B\)BQ�BK�BC�B@�B=qB5?B-BuB
�B
��B
�qB
�'B
��B
��B
��B
��B
�uB
�=B
{�B
p�B
O�B
H�B
7LB
#�B
hB
B
  B	��B	��B	��B	�B	�B	�sB	�BB	�)B	��B	ĜB	�}B	�jB	�FB	�3B	��B	��B	��B	��B	�{B	�B	y�B	bNB	XB	VB	Q�B	J�B	A�B	8RB	&�B	�B	PB	JB	
=B	
=B	1B	B	B	B	B��B��B��B��B�B�B�mB�/B�#B�B�B�B�
B��B��B��BȴB��B�qB�dB�RB�LB�?B�FB�?B�9B�3B�B�B��B��B��B��B��B��B��B�uB�uB�hB�bB�DB�+B�B{�Bv�Bs�Bq�Bm�Bk�BjBiyBhsBgmBgmBe`BcTBYBQ�BO�BN�BL�BJ�BH�BD�BC�B@�B?}B?}B<jB<jB<jB<jB;dB:^B9XB8RB8RB5?B7LB2-B1'B/B0!B2-B1'B/B/B0!B0!B0!B0!B33B7LB7LB8RB8RB9XB9XB9XB>wB?}B=qB;dB<jB=qB<jB<jB@�BD�BH�BJ�BK�BK�BL�BL�BL�BP�BP�BQ�BR�BS�BS�BS�BW
BXBXBYB[#B[#B[#B[#B\)B^5BcTBcTBcTBdZBdZBdZBe`BhsBk�Bk�Bk�Bl�Bl�Bm�Bo�Bp�Bt�Bv�Bw�Bw�By�By�Bz�B�B�B�%B�%B�%B�%B�1B�JB�\B�hB�uB�{B��B��B��B��B��B��B��B�B�B�B�'B�LB�^B�jB�}BÖBƨB��B��B��B��B�B�B�)B�;B�NB�`B�mB�sB�B�B�B�B�B��B��B��B	B	B		7B	JB	VB	�B	�B	 �B	#�B	'�B	)�B	,B	+B	+B	,B	,B	-B	0!B	49B	7LB	9XB	:^B	A�B	H�B	H�B	K�B	M�B	O�B	S�B	YB	ZB	]/B	_;B	`BB	e`B	gmB	hsB	jB	iyB	iyB	k�B	n�B	p�B	q�B	q�B	q�B	q�B	q�B	r�B	t�B	w�B	z�B	{�B	|�B	~�B	~�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�+B	�7B	�=B	�DB	�\B	�hB	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�-B	�9B	�?B	�FB	�RB	�RB	�RB	�^B	�qB	�}B	�}B	��B	ÖB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�#B	�#B	�#B	�B	�B	�B	�B	�#B	�#B	�B	�?B
'B
PB

B
"�B
+�B
1�B
9�B
B�B
G�B
L�B
R:B
V�B
]/B
cnB
gmB
kB
o�B
s�B
uZ111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  Bh}Bl�BB�-B�@B�@B�LB��B��B�B�&B�,B�,B�KB��B*BO�B\"Bp�Br�Bt�By�B|�B�@B�|B��B�B��B��B�/B�/B�(B�;BǢBʶB�sB�[B�7B�B��B��B��B� B�B��B��B��B��B��B��B��B��B�qB�B~�Bv�Bq�Bo�Bm�Bl�BixBfgBL�B<nB6JB4>B1+B0%B.B�B�B�BWB	@B��B�eB�.B��BǻBĩB�NB�zBs�Bh�BSBSBH�BB�B:wB7eB4SB,"B#�B
ZB
�B
ľB
�]B
�B
��B
��B
��B
��B
�eB
�-B
r�B
g�B
F�B
?�B
.CB
�B
bB	�B	��B	��B	��B	��B	�B	�B	�qB	�AB	�(B	��B	��B	�~B	�lB	�HB	�5B	��B	��B	��B	��B	��B	{B	p�B	YWB	OB	MB	H�B	A�B	8�B	/_B	�B	�B	`B	[B	NB	NB�BB�0B�+B�B�B� B��B��B��B��B�BށB�DB�8B�3B�-B�&B� B��B��B��B��B��B��B�}B�kB�eB�XB�_B�XB�RB�MB�.B�"B�B��B��B��B��B��B��B��B��B��B�B�aB~IBz0BsBm�Bj�Bh�Bd�Bb�Ba�B`�B_�B^�B^�B\�BZvBP:BIBGBE�BC�BA�B?�B;�B:�B7�B6�B6�B3�B3�B3�B3�B2�B1�B0~B/yB/yB,fB.sB)TB(OB&CB'IB)UB(OB&CB&CB'IB'IB'IB'IB*[B.tB.tB/zB/zB0�B0�B0�B5�B6�B4�B2�B3�B4�B3�B3�B7�B;�B?�BA�BB�BB�BC�BC�BC�BHBHBIBJBK BK BK BN2BO8BO8BP?BRKBRKBRKBRKBSQBU]BZ|BZ|BZ|B[�B[�B[�B\�B_�Bb�Bb�Bb�Bc�Bc�Bd�Bf�Bg�Bk�Bm�Bn�Bn�BqBqBrBz9B|EB}KB}KB}KB}KBWB�pB��B��B��B��B��B��B��B��B��B��B�B�'B�-B�3B�KB�pB��B��B��B��B��B��B��B��B�B�9B�?B�KB�]B�oB܁BގBߔB�B�B�B��B��B�B�B�B�+B�+B	 VB	iB	uB	�B	�B	�B	�B	B	!B	#%B	"B	"B	#%B	#%B	$+B	'=B	+UB	.hB	0tB	1zB	8�B	?�B	?�B	B�B	D�B	F�B	KB	P1B	Q7B	TIB	VUB	W\B	\yB	^�B	_�B	a�B	`�B	`�B	b�B	e�B	g�B	h�B	h�B	h�B	h�B	h�B	i�B	k�B	n�B	q�B	r�B	tB	vB	vB	xB	y$B	y$B	y$B	y$B	z*B	z*B	z*B	{1B	|6B	{1B	|7B	|7B	~BB	�NB	�TB	�[B	�sB	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�)B	�<B	�BB	�NB	�TB	�ZB	�fB	�fB	�fB	�rB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�"B	�/B	�5B	�5B	�5B	�/B	�/B	�/B	�/B	�5G�O�B	ښB	�PB	�7B
`B
B
�B
"�B
)B
0�B
9�B
>�B
C�B
IHB
M�B
T<B
Z{B
^zB
b)B
f�B
j�B
lg111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.02 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9998(+/-0.0001), vertically averaged dS =-0.009(+/-0.003) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144112022020411441120220204114411  AO  ARCAADJP                                                                    20200618141337    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200618141337  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200618141337  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114411  IP                  G�O�G�O�G�O�                