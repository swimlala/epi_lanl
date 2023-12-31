CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-11-21T12:58:27Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         (Matthew Alkire, University of Washington      @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7,   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  74   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7t   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     88   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8X   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8x   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           8|   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
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
_FillValue                    �|   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �|   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �t   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �    HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �0   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �4   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �D   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �H   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �L   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20181121125827  20190408133246  5904461 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  5286                            2C  D   APEX                            6531                            072314                          846 @��T��Q21   @��U�/s@55?|��b�C��1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   A   A   @9��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  BpffBx��B��B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+�fD,fD,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Diy�Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dtl�Dyy�D� D�S3D�s3D��3D�3D�FfD��fD�ɚD��D�FfD��fD�� D�  D�6fD�vfD��3D�fD�C3D�c3D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @8Q�@~�R@�\)@�\)A�A?�A_�A�A��
A��
A��
A��
A��
A��
A��
A��
B�B�B�B�B'�B/�B7�B?�BG�BO�BW�B_�Bg�BpQ�Bx�RB�B���B���B���B���B���B���B�(�B�(�B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC�
>C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qD ~�D ��D~�D��D~�D��D~�D��D~�D��D~�D��D�D��D~�D��D~�D��D	~�D	��D
~�D
��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D ~�D ��D!~�D!��D"~�D"��D#~�D#��D$~�D$��D%~�D%��D&~�D&��D'~�D'��D(~�D(��D)~�D)��D*~�D*��D+�D,D,~�D,��D-~�D-��D.~�D.��D/~�D/��D0~�D0��D1~�D1��D2~�D2��D3~�D3��D4~�D4��D5~�D5��D6~�D6��D7~�D7��D8~�D8��D9~�D9��D:~�D:��D;~�D;��D<~�D<��D=~�D=��D>~�D>��D?~�D?��D@~�D@��DA~�DA��DB~�DB��DC~�DC��DD~�DD��DE~�DE��DF~�DF��DG~�DG��DH~�DH��DI~�DI��DJ~�DJ��DK~�DK��DL~�DL��DM~�DM��DN~�DN��DO~�DO��DP~�DP��DQ~�DQ��DR~�DR��DS~�DS��DT~�DT��DU~�DU��DV~�DV��DW~�DW��DX~�DX��DY~�DY��DZ~�DZ��D[~�D[��D\~�D\��D]~�D]��D^~�D^��D_~�D_��D`~�D`��Da~�Da��Db~�Db��Dc~�Dc��Dd~�Dd��De~�De��Df~�Df��Dg~�Dg��Dh~�Dh��DixRDi��Dj~�Dj��Dk~�Dk��Dl~�Dl��Dm~�Dm��Dn~�Dn��Do~�Do��Dp~�Dp��Dq~�Dq��Dr~�Dr��Ds~�Ds��Dtk�DyxRD�\D�R�D�r�D�ҏD��D�E�D���D���D�)D�E�D���D��\D��\D�5�D�u�D�D��D�B�D�b�D��)111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��Aպ^AռjAռjAմ9Aղ-AծAհ!Aպ^AռjAռjAվwAվwAռjAռjA՛�A�{A�  A�&�A�^5A�;dA�"�A��A�{A�bA�
=A���A��A��/A�ĜAџ�A�%A��A� �A�\)Aǉ7A��mA��Aŧ�A�;dA��A��A��!A��A�;dA���A��TA���A��A��A���A���A��A���A�Q�A���A��DA���A���A�`BA���A��/A�\)A��A�E�A��wA�7LA�1'A�JA�jA���A�bA�ZA��A��FA� �A���A�
=A�33A�"�A��`A�K�A�A�9XA���A�ZA��+A��A�A�A�{A�z�A�bA��A��A��jA���A���A��A�|�A�|�A���A��#A�XA�~�A�z�A�^5A���A�E�A��+A�O�A�AXA~�\A|ffAx=qAsdZAqVAo\)An��An-Ak\)Ai�PAg+AdI�Act�AbZA`v�A]ƨAZ�\AXQ�AV�`AT��AR�+AO��AM��AKG�AIƨAH�AH  AFM�AD�\AB~�AA�^A@v�A?&�A>  A<�A;S�A:��A8�`A77LA5
=A3p�A1�^A0��A/|�A-�
A-%A+O�A*^5A(��A(1'A'�hA&�9A%ƨA$ZA#\)A!hsA�hA~�A�RAoA�
A��A�A�DA�7AJA��A(�AbA
=Av�A��A�\A{AƨA�9Ax�A�A�#AVA	|�A	oA�Az�A�^A;dA��Al�A��A��AM�A��A ��A �D@�$�@�bN@�o@��#@�ff@�+@��R@���@�p�@�&�@�V@���@��@�@��@��@�1'@���@��^@���@��`@�j@��@�I�@�(�@�Z@�j@�l�@�@�{@���@��@�X@��@��@�ƨ@ݺ^@��@��@��@�(�@�C�@�E�@ٙ�@�Ĝ@�b@�\)@֧�@�@���@� �@�|�@��@љ�@�7L@��m@�l�@�+@Η�@Ͳ-@��@̣�@�9X@��@̋D@̼j@̣�@̃@�j@��;@�\)@�"�@ʗ�@�=q@�J@�%@�1'@� �@Ǖ�@���@�=q@Ł@�p�@�O�@�/@��@Ĵ9@�(�@�1@öF@�33@���@�v�@�V@��@�G�@�&�@��@��@�A�@�1@��;@��P@�;d@��@�ȴ@�^5@��T@�@��#@��-@�x�@���@��9@�I�@�\)@��R@��\@�E�@�J@���@��@�v�@�^5@�V@�9X@�ƨ@�I�@��`@�%@���@�9X@���@��@��D@��y@�%@�1'@��@��@���@�V@��@��@��9@��u@�r�@�Z@�1@��;@�|�@�K�@���@���@�E�@�5?@��@��^@��^@���@���@�33@���@�
=@���@�@��#@�=q@���@�&�@��`@���@�1@�9X@��u@�1@�1'@��@�C�@�K�@�\)@�dZ@�o@���@�@��@��@�-@�5?@�$�@�{@���@���@�X@���@�j@�z�@�1'@���@�K�@�
=@���@��@���@�-@���@��T@���@���@�`B@���@�j@��@���@�+@�
=@�@���@��`@��@�1'@�S�@���@���@�~�@���@��@��R@���@�=q@��7@�7L@��@��@���@���@��`@��/@���@���@�Ĝ@���@���@�\)@�C�@�+@�o@���@��H@���@�ff@�@�x�@�&�@���@��`@���@���@�j@�1'@�1@��w@�S�@��H@�ȴ@��R@���@���@���@�~�@�n�@�ff@�M�@�$�@��@��-@��@�G�@�&�@��@���@��@�I�@�1@��T@���@��@uO�@n$�@cƨ@]p�@Tz�@M��@Fff@A%@9x�@2�!@-@(Q�@#�F@�@��@V@��@�j111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  Aպ^AռjAռjAմ9Aղ-AծAհ!Aպ^AռjAռjAվwAվwAռjAռjA՛�A�{A�  A�&�A�^5A�;dA�"�A��A�{A�bA�
=A���A��A��/A�ĜAџ�A�%A��A� �A�\)Aǉ7A��mA��Aŧ�A�;dA��A��A��!A��A�;dA���A��TA���A��A��A���A���A��A���A�Q�A���A��DA���A���A�`BA���A��/A�\)A��A�E�A��wA�7LA�1'A�JA�jA���A�bA�ZA��A��FA� �A���A�
=A�33A�"�A��`A�K�A�A�9XA���A�ZA��+A��A�A�A�{A�z�A�bA��A��A��jA���A���A��A�|�A�|�A���A��#A�XA�~�A�z�A�^5A���A�E�A��+A�O�A�AXA~�\A|ffAx=qAsdZAqVAo\)An��An-Ak\)Ai�PAg+AdI�Act�AbZA`v�A]ƨAZ�\AXQ�AV�`AT��AR�+AO��AM��AKG�AIƨAH�AH  AFM�AD�\AB~�AA�^A@v�A?&�A>  A<�A;S�A:��A8�`A77LA5
=A3p�A1�^A0��A/|�A-�
A-%A+O�A*^5A(��A(1'A'�hA&�9A%ƨA$ZA#\)A!hsA�hA~�A�RAoA�
A��A�A�DA�7AJA��A(�AbA
=Av�A��A�\A{AƨA�9Ax�A�A�#AVA	|�A	oA�Az�A�^A;dA��Al�A��A��AM�A��A ��A �D@�$�@�bN@�o@��#@�ff@�+@��R@���@�p�@�&�@�V@���@��@�@��@��@�1'@���@��^@���@��`@�j@��@�I�@�(�@�Z@�j@�l�@�@�{@���@��@�X@��@��@�ƨ@ݺ^@��@��@��@�(�@�C�@�E�@ٙ�@�Ĝ@�b@�\)@֧�@�@���@� �@�|�@��@љ�@�7L@��m@�l�@�+@Η�@Ͳ-@��@̣�@�9X@��@̋D@̼j@̣�@̃@�j@��;@�\)@�"�@ʗ�@�=q@�J@�%@�1'@� �@Ǖ�@���@�=q@Ł@�p�@�O�@�/@��@Ĵ9@�(�@�1@öF@�33@���@�v�@�V@��@�G�@�&�@��@��@�A�@�1@��;@��P@�;d@��@�ȴ@�^5@��T@�@��#@��-@�x�@���@��9@�I�@�\)@��R@��\@�E�@�J@���@��@�v�@�^5@�V@�9X@�ƨ@�I�@��`@�%@���@�9X@���@��@��D@��y@�%@�1'@��@��@���@�V@��@��@��9@��u@�r�@�Z@�1@��;@�|�@�K�@���@���@�E�@�5?@��@��^@��^@���@���@�33@���@�
=@���@�@��#@�=q@���@�&�@��`@���@�1@�9X@��u@�1@�1'@��@�C�@�K�@�\)@�dZ@�o@���@�@��@��@�-@�5?@�$�@�{@���@���@�X@���@�j@�z�@�1'@���@�K�@�
=@���@��@���@�-@���@��T@���@���@�`B@���@�j@��@���@�+@�
=@�@���@��`@��@�1'@�S�@���@���@�~�@���@��@��R@���@�=q@��7@�7L@��@��@���@���@��`@��/@���@���@�Ĝ@���@���@�\)@�C�@�+@�o@���@��H@���@�ff@�@�x�@�&�@���@��`@���@���@�j@�1'@�1@��w@�S�@��H@�ȴ@��R@���@���@���@�~�@�n�@�ff@�M�@�$�@��@��-@��@�G�@�&�@��@���@��@�I�@�1@��T@���@��@uO�@n$�@cƨ@]p�@Tz�@M��@Fff@A%@9x�@2�!@-@(Q�@#�F@�@��@V@��@�j111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
P�B
P�B
P�B
P�B
P�B
Q�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
Q�B
W
B
`BB
ffB
iyB
jB
iyB
iyB
iyB
jB
jB
jB
iyB
iyB
jB
iyB
l�B
�oB
�B
�wB
�`B
�BDB�B9XBH�BbNBcTBl�Bl�Bu�B�B�\B��B��B�dB�B�BB�/B�B�TB�B�B�B��B+BVB\BhB�B!�B�B�B�BB�BB�B��B�B
=B7LB+B0!B>wBgmB�B�=By�Bo�BbNBdZBK�B"�B��B�BB�!B��B~�BcTBk�B^5BG�B'�B�B,BC�B#�BVB
�HB
�PB
H�B
�B
\B	��B	�NB	��B	��B	�B	�B	�
B	�B	�B	n�B	_;B	]/B	ffB	o�B	`BB	M�B	C�B	G�B	E�B	:^B	.B	�B	VB	B��B�B�BB�B��BǮBƨBB�XB�9B�'B�!B�B�B�B�!B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�bB�=B�%B�B� B�B� B~�B~�B~�B{�Bu�Bt�Bx�Bu�Bt�Bq�Bn�Bl�Bm�BhsBdZB`BB^5B\)BZB^5Bm�Bt�Bs�Bu�Bt�Bo�BjBgmBffBhsBjBq�Bk�BhsBhsBgmBq�B}�B�B�B�%B�1B�1B�1B�1B�+B�B�B|�Bu�B�B�PB�PB�\B�1Bz�B~�B�B�DB�bB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�'B�3B�LB�dB��BŢBǮB��B��B�B�
B�5B�HB�`B�mB�B�B��B��B��B	1B	DB	PB	PB	PB	PB	JB	PB	VB	VB	bB	hB	�B	�B	�B	�B	�B	�B	&�B	)�B	0!B	1'B	5?B	8RB	8RB	;dB	?}B	B�B	B�B	B�B	B�B	B�B	D�B	F�B	K�B	M�B	O�B	P�B	Q�B	S�B	S�B	W
B	W
B	YB	\)B	^5B	cTB	dZB	e`B	gmB	iyB	jB	l�B	n�B	p�B	p�B	o�B	cTB	VB	VB	W
B	dZB	e`B	k�B	p�B	r�B	s�B	q�B	n�B	iyB	e`B	aHB	^5B	]/B	`BB	_;B	`BB	aHB	`BB	bNB	ffB	gmB	gmB	iyB	k�B	r�B	t�B	u�B	w�B	x�B	z�B	~�B	�B	�B	�+B	�B	�B	�B	�B	�1B	�1B	�+B	�7B	�PB	�VB	�JB	�JB	�DB	�=B	�PB	�uB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�'B	�!B	�!B	�'B	�9B	�LB	�FB	�?B	�FB	�FB	�LB	�XB	�^B	�dB	��B	��B	��B	ɺB	ǮB	ǮB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�
B	�B	�
B	�B	�B	�B	�B	�B	�B	�
B	�B	�B	�B	�B	�B	�/B	�5B	�;B	�;B	�BB	�HB	�TB	�ZB	�`B	�fB	�mB	�mB	�sB	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B
B
PB
oB
�B
�B
&�B
.B
8RB
=qB
C�B
I�B
M�B
Q�B
W
B
\)B
bNB
gmB
l�B
q�B
t�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
P�B
P�B
P�B
P�B
P�B
Q�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
Q�B
W
B
`CB
fhB
i{B
j�B
i{B
izB
i}B
j�B
j�B
j�B
i{B
i}B
j�B
i{B
l�B
�pB
�B
�xB
�aB
�BGB�B9YBH�BbNBcXBl�Bl�Bu�B�B�^B��B��B�eB�B�EB�/B�B�TB�B�B�B��B,BYB[BlB�B!�B�B�B�BB�EB�B��B�B
@B7MB+B0$B>zBgrB�B�@By�Bo�BbNBdYBK�B"�B��B�CB�#B��B~�BcVBk�B^5BG�B'�B�B,	BC�B#�BXB
�JB
�QB
H�B
�B
\B	��B	�MB	��B	��B	�B	�B	�B	�B	�B	n�B	_<B	]2B	fkB	o�B	`CB	M�B	C�B	G�B	E�B	:`B	.B	�B	UB	B��B�B�AB�B��BǰBƫBB�\B�=B�)B�#B�B�	B�	B�#B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�fB�?B�)B�"B�B�
B�B~�B~�B~�B{�Bu�Bt�Bx�Bu�Bt�Bq�Bn�Bl�Bm�BhtBd[B`DB^9B\,BZB^7Bm�Bt�Bs�Bu�Bt�Bo�Bj�BgpBfhBhuBj�Bq�Bk�BhuBhvBgpBq�B}�B�B� B�*B�5B�2B�3B�4B�-B�#B�B|�Bu�B�B�PB�QB�\B�6Bz�B~�B�B�FB�dB�}B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�(B�5B�OB�gB��BťBǰB��B��B�B�B�5B�JB�cB�pB�B�B��B��B��B	2B	GB	QB	QB	QB	QB	LB	TB	WB	WB	eB	jB	�B	�B	�B	�B	�B	�B	&�B	)�B	0'B	1)B	5AB	8TB	8UB	;fB	?}B	B�B	B�B	B�B	B�B	B�B	D�B	F�B	K�B	M�B	O�B	P�B	Q�B	S�B	S�B	WB	WB	YB	\,B	^7B	cWB	d[B	edB	goB	i{B	j�B	l�B	n�B	p�B	p�B	o�B	cVB	VB	V	B	WB	d[B	ecB	k�B	p�B	r�B	s�B	q�B	n�B	i|B	ebB	aIB	^8B	]3B	`DB	_=B	`EB	aLB	`EB	bQB	fiB	gpB	gqB	i}B	k�B	r�B	t�B	u�B	w�B	x�B	z�B	~�B	�B	�!B	�/B	�B	�B	�B	�"B	�7B	�5B	�-B	�9B	�SB	�YB	�NB	�MB	�GB	�@B	�RB	�wB	�xB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�*B	�&B	�#B	�(B	�:B	�QB	�GB	�BB	�IB	�IB	�NB	�[B	�`B	�gB	��B	��B	��B	ɻB	ǱB	ǯB	ǰB	ȶB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�
B	�B	�B	�B	�B	�B	�0B	�5B	�@B	�?B	�DB	�JB	�UB	�\B	�cB	�gB	�qB	�oB	�uB	�zB	�zB	�{B	�{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B
B
SB
qB
�B
�B
&�B
.B
8SB
=rB
C�B
I�B
M�B
Q�B
WB
\*B
bPB
goB
l�B
q�B
t�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.02 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             201904081332462019040813324620190408133246  AO  ARCAADJP                                                                    20181121125827    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181121125827  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181121125827  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190408133246  IP                  G�O�G�O�G�O�                