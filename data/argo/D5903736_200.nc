CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2017-11-01T15:05:23Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         (Matthew Alkire, University of Washington      @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
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
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20171101150523  20190604094030  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  4051                            2C  D   APEX                            5368                            041511                          846 @�1��
��1   @�1�[f�@5r-V�d�
=p��1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B���B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C�C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$�fD%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt� Dy� D��D�C�D�� D��D�)D�7�D���D���D�
=D�/
D�a�D�θD��D�>fD�pRD�k�D��D�4)D�l�D��3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @~�R@�\)@�\)A�A?�A_�A�A��
A��
A��
A��
A��
A��
A��
A��
B�B�B�B�B'�B/�B7�B?�BG�BO�BW�B_�Bg�Bo�Bw�B�B�(�B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C{C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qD ~�D ��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D	~�D	��D
~�D
��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D ~�D ��D!~�D!��D"~�D"��D#~�D#��D$�D$��D%~�D%��D&~�D&��D'~�D'��D(~�D(��D)~�D)��D*~�D*��D+~�D+��D,~�D,��D-~�D-��D.~�D.��D/~�D/��D0~�D0��D1~�D1��D2~�D2��D3~�D3��D4~�D4��D5~�D5��D6~�D6��D7~�D7��D8~�D8��D9~�D9��D:~�D:��D;~�D;��D<~�D<��D=~�D=��D>~�D>��D?~�D?��D@~�D@��DA~�DA��DB~�DB��DC~�DC��DD~�DD��DE~�DE��DF~�DF��DG~�DG��DH~�DH��DI~�DI��DJ~�DJ��DK~�DK��DL~�DL��DM~�DM��DN~�DN��DO~�DO��DP~�DP��DQ~�DQ��DR~�DR��DS~�DS��DT~�DT��DU~�DU��DV~�DV��DW~�DW��DX~�DX��DY~�DY��DZ~�DZ��D[~�D[��D\~�D\��D]~�D]��D^~�D^��D_~�D_��D`~�D`��Da~�Da��Db~�Db��Dc~�Dc��Dd~�Dd��De~�De��Df~�Df��Dg~�Dg��Dh~�Dh��Di~�Di��Dj~�Dj��Dk~�Dk��Dl~�Dl��Dm~�Dm��Dn~�Dn��Do~�Do��Dp~�Dp��Dq~�Dq��Dr~�Dr��Ds~�Ds��Dt~�Dt޸Dy��D�HD�C3D��\D��pD��D�7
D���D��3D�	�D�.fD�`�D��D�
D�=�D�o�D�j�D��D�3�D�l)D��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�Q�A�VA�VA�VA�VA�VA�S�A�XA�ZA�\)A�ZA�ZA�XA�XA�p�A�v�A�jA�O�A�K�A�I�A�A�A�A�A�;dA�9XA�&�A�  A���A���A֥�AցAӟ�A�x�A�x�A�-AŃA��;A�  A�A���A�1'A��-A�%A���A� �A�bA� �A�A�A�?}A�%A�v�A��#A�`BA���A�=qA��+A���A���A�&�A�5?A�l�A��A�z�A��A�1A�`BA���A��A��#A�I�A��^A��yA���A���A���A��hA�
=A���A�/A��A��A��-A�hsA�dZA��A�A�bA��wA��yA���A��FA�jA��A��jA�^5A�`BA�G�A��A���A��A�5?A��/A�\)A��A�-A�VA��9A�v�A�t�A��RA��^A�E�A�1'A�{A��FA�A�A���A�-A�I�A\)A~1'A|jAy��AvjAtz�Ar��Aq33Ap�An��An1'Al�yAk|�Aj�uAhjAf�jAe&�Ad9XAc7LA`n�A^A�A\��A[C�AZ~�AY?}AWAU�AU?}AS��ARbAPr�AO�ANI�AK�
AI�mAGAF�AEt�ACx�AA��A?��A?C�A>VA="�A;�
A:�\A9�FA7��A6ZA4ĜA4ffA4E�A3�;A3
=A2ffA0��A/t�A.�!A-�A,��A,1'A,A+��A+G�A*��A*M�A'�;A'33A&�jA%l�A#�A"�A!�mA {A��A�jA�A�#Al�A�9A�A��A��A��A�#AC�A�A�A|�A��AffA�yA
��A	G�AA�yA9XA��A-A��A�RA�A��A�wAx�A  �@�`B@��9@���@�O�@�Ĝ@��@�+@�Ĝ@�1'@�Ĝ@��y@�z�@�A�@�X@��@��@�Ĝ@�r�@�(�@��m@�@�+@�+@◍@�M�@��@�9@�Ĝ@���@���@�9@�Q�@��y@�V@���@�O�@���@�ȴ@ف@؃@׾w@�l�@�v�@ԋD@���@�hs@�j@϶F@�E�@��T@��@�  @�@�=q@�hs@�Q�@ǍP@�o@ƸR@�n�@�$�@�{@��@�hs@�I�@�ȴ@+@�v�@��@�G�@�
=@��@���@�7L@�z�@�Q�@� �@���@�"�@��y@���@��@��D@�A�@�1@��
@��P@�
=@�E�@���@�hs@��/@��u@�bN@� �@���@�S�@��@���@��@���@��
@�
=@���@�=q@�`B@��@��@�Q�@���@�o@���@���@�hs@�&�@��@��u@��w@�\)@�;d@�o@��@��H@�ȴ@��R@���@�V@�`B@��`@���@��u@�bN@�(�@��@�K�@�"�@��@���@�E�@��@�@���@��h@���@���@�x�@�/@�j@��;@�\)@�+@��@�-@��@�%@�Z@� �@�b@�  @���@��F@���@���@��@�l�@�l�@�;d@���@��R@��R@���@�M�@�$�@��@�O�@�&�@���@�Ĝ@��9@��@�bN@� �@���@�S�@�K�@�@�{@��^@��7@�/@��@��@���@��/@��@�Q�@�  @��F@��P@�dZ@�33@���@���@��+@�n�@�M�@�{@��^@���@��h@�7L@��/@��j@�r�@�Q�@�I�@�A�@�A�@�9X@��@�K�@�o@��@�~�@�-@��T@�@���@��7@��@��@�A�@�  @��m@��w@�\)@�C�@�;d@�"�@��@��R@�v�@�V@�5?@�{@��@��^@���@���@���@���@�O�@�V@��/@��j@���@��u@��D@��@�bN@�  @���@�K�@�+@�o@�v`@��1@{{J@s��@l��@e%@[U�@U�h@M�Z@H�.@EA @=�'@7��@/@(�4@#l�@��@B[@m]@��@W�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A�Q�A�VA�VA�VA�VA�VA�S�A�XA�ZA�\)A�ZA�ZA�XA�XA�p�A�v�A�jA�O�A�K�A�I�A�A�A�A�A�;dA�9XA�&�A�  A���A���A֥�AցAӟ�A�x�A�x�A�-AŃA��;A�  A�A���A�1'A��-A�%A���A� �A�bA� �A�A�A�?}A�%A�v�A��#A�`BA���A�=qA��+A���A���A�&�A�5?A�l�A��A�z�A��A�1A�`BA���A��A��#A�I�A��^A��yA���A���A���A��hA�
=A���A�/A��A��A��-A�hsA�dZA��A�A�bA��wA��yA���A��FA�jA��A��jA�^5A�`BA�G�A��A���A��A�5?A��/A�\)A��A�-A�VA��9A�v�A�t�A��RA��^A�E�A�1'A�{A��FA�A�A���A�-A�I�A\)A~1'A|jAy��AvjAtz�Ar��Aq33Ap�An��An1'Al�yAk|�Aj�uAhjAf�jAe&�Ad9XAc7LA`n�A^A�A\��A[C�AZ~�AY?}AWAU�AU?}AS��ARbAPr�AO�ANI�AK�
AI�mAGAF�AEt�ACx�AA��A?��A?C�A>VA="�A;�
A:�\A9�FA7��A6ZA4ĜA4ffA4E�A3�;A3
=A2ffA0��A/t�A.�!A-�A,��A,1'A,A+��A+G�A*��A*M�A'�;A'33A&�jA%l�A#�A"�A!�mA {A��A�jA�A�#Al�A�9A�A��A��A��A�#AC�A�A�A|�A��AffA�yA
��A	G�AA�yA9XA��A-A��A�RA�A��A�wAx�A  �@�`B@��9@���@�O�@�Ĝ@��@�+@�Ĝ@�1'@�Ĝ@��y@�z�@�A�@�X@��@��@�Ĝ@�r�@�(�@��m@�@�+@�+@◍@�M�@��@�9@�Ĝ@���@���@�9@�Q�@��y@�V@���@�O�@���@�ȴ@ف@؃@׾w@�l�@�v�@ԋD@���@�hs@�j@϶F@�E�@��T@��@�  @�@�=q@�hs@�Q�@ǍP@�o@ƸR@�n�@�$�@�{@��@�hs@�I�@�ȴ@+@�v�@��@�G�@�
=@��@���@�7L@�z�@�Q�@� �@���@�"�@��y@���@��@��D@�A�@�1@��
@��P@�
=@�E�@���@�hs@��/@��u@�bN@� �@���@�S�@��@���@��@���@��
@�
=@���@�=q@�`B@��@��@�Q�@���@�o@���@���@�hs@�&�@��@��u@��w@�\)@�;d@�o@��@��H@�ȴ@��R@���@�V@�`B@��`@���@��u@�bN@�(�@��@�K�@�"�@��@���@�E�@��@�@���@��h@���@���@�x�@�/@�j@��;@�\)@�+@��@�-@��@�%@�Z@� �@�b@�  @���@��F@���@���@��@�l�@�l�@�;d@���@��R@��R@���@�M�@�$�@��@�O�@�&�@���@�Ĝ@��9@��@�bN@� �@���@�S�@�K�@�@�{@��^@��7@�/@��@��@���@��/@��@�Q�@�  @��F@��P@�dZ@�33@���@���@��+@�n�@�M�@�{@��^@���@��h@�7L@��/@��j@�r�@�Q�@�I�@�A�@�A�@�9X@��@�K�@�o@��@�~�@�-@��T@�@���@��7@��@��@�A�@�  @��m@��w@�\)@�C�@�;d@�"�@��@��R@�v�@�V@�5?@�{@��@��^@���@���@���@���@�O�@�V@��/@��j@���@��u@��D@��@�bN@�  @���@�K�@�+G�O�@�v`@��1@{{J@s��@l��@e%@[U�@U�h@M�Z@H�.@EA @=�'@7��@/@(�4@#l�@��@B[@m]@��@W�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B��B��B��B��B��B��B��B��B��B��B��B��BB1B
=B�B�B�B#�B%�B'�B,B0!B33B33B49B33B0!B49B=qB1'B!�B�B�B!�B"�B%�B'�B)�B+B?}BF�BO�B]/BgmBm�Bx�B�B�VB�JB�B�+B�bB��B��B��B��B��B�{B�uB�\B�+B�B�B�B�B~�B{�By�By�By�Bz�By�By�Bu�Br�Bl�BaHBR�BG�BO�BYBQ�BC�B+B%B��B�B�BB�5B�#B�)B�/B�)B�B��BɺBB�^B��B�BdZBN�B-B�B	7B
�B
�TB
�B
��B
�wB
�RB
�!B
��B
�hB
y�B
o�B
e`B
W
B
C�B
(�B
�B
bB
%B	��B	��B	�B	�B	�NB	�B	��B	B	�RB	�'B	��B	��B	�=B	�B	y�B	s�B	m�B	dZB	\)B	W
B	O�B	F�B	>wB	:^B	33B	(�B	�B	�B	bB	
=B	B��B�B�B�B�fB�HB�)B�B��B��BȴBƨBŢBÖB��B�qB�XB�?B�3B�'B�B�B�B��B��B��B��B��B��B��B��B��B�oB�VB�=B�%B�B�B�B~�B|�Bz�Bx�Bw�Bu�Bt�Br�Bq�Bn�Bl�BiyBgmBcTBaHBcTBgmBgmBffBe`BbNBbNBffBgmBgmBgmBffBgmBdZBaHB_;BaHBgmBk�Bk�BhsBl�Bs�Bo�BhsB_;B^5BaHBe`Be`Be`Be`Be`Be`BffBe`BffBe`BgmBiyBjBp�Br�Bs�Bw�B}�B� B�B�B�B�%B�+B�7B�DB�PB�bB�uB�uB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�!B�B�B�B�-B�9B�9B�-B�B�B�B�B�!B�!B�!B�3B�FB�dB�qB��B��BÖBŢBƨBȴB��B��B��B��B�B�B�B�#B�;B�BB�TB�B�B�B��B��B	  B	B	PB	\B	bB	{B	�B	�B	 �B	'�B	+B	-B	.B	0!B	7LB	:^B	;dB	<jB	=qB	=qB	>wB	>wB	?}B	@�B	F�B	I�B	J�B	J�B	K�B	L�B	O�B	Q�B	R�B	T�B	VB	YB	\)B	]/B	^5B	_;B	aHB	cTB	gmB	iyB	m�B	n�B	p�B	p�B	q�B	x�B	{�B	}�B	�B	�B	�B	�B	�B	�%B	�1B	�DB	�DB	�VB	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	�-B	�-B	�3B	�XB	�jB	�wB	B	ÖB	ÖB	ĜB	ƨB	ȴB	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�#B	�#B	�)B	�)B	�)B	�5B	�BB	�BB	�NB	�TB	�ZB	�ZB	�`B	�mB	�mB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
  B
  B
B
B
B
B
B
	�B
MB
�B
)yB
/5B
3�B
@�B
EB
JXB
N�B
Q�B
X�B
]/B
cB
g�B
i�B
oOB
t�B
yrB
|�B
H111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B�B�B�B��B��B��B��B��B�B�B�B�B�B��B��B��B�B
PBjB|B�B�B�B�B#�B&�B&�B( B&�B#�B( B16B$�B�B�B�B�B�B�B�B�B�B3EB:qBC�BP�B[5BaTBl�Bt�B�B�Bx�Bz�B�$B�[B�FB�SB�SB�GB�>B�8B�"Bz�Bw�Bw�Bv�Bu�Br�Bo�Bm�Bm�Bm�Bn�Bm�Bm�Bi�BfwB`SBUBF�B;{BC�BL�BE�B7fB�B��B��B�sB�B�B� B�B�B�B��B��B��B�mB�?B�xBt�BXABB�B �B�B
�+B
�B
�LB
�B
��B
�rB
�MB
�B
��B
�mB
m�B
c�B
YaB
KB
7�B
 B
�B
oB	�/B	�B	��B	��B	ލB	�^B	�*B	��B	��B	�jB	�@B	�B	��B	~XB	v%B	m�B	g�B	a�B	XwB	PKB	K+B	C�B	:�B	2�B	.�B	'WB	B	�B	�B	�B�bB�2B�B��B��BެBڑB�zB�ZB�BB�B� B��B��B��B��B��B��B��B�uB�jB�ZB�LB�?B�8B�/B�-B� B�B��B��B��B��B��B��B��B~vBzaBxUBvIBuBBs:Bq*Bo BmBlBj Bh�Bf�Be�Bb�B`�B]�B[�BW�BU�BW�B[�B[�BZ�BY�BV�BV�BZ�B[�B[�B[�BZ�B[�BX�BU�BS�BU�B[�B_�B_�B\�B`�Bg�Bc�B\�BS�BRzBU�BY�BY�BY�BY�BY�BY�BZ�BY�BZ�BY�B[�B]�B^�Bd�Bf�Bg�BlBr8BtCBuJBvOBx\BzhB{mB}zB�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�7B�;B�GB�RB�bB�^B�QB�]B�mB�zB�zB�kB�RB�GB�SB�NB�cB�dB�aB�uB��B��B��B��B��B��B��B��B��B�	B�B�.B�<B�SB�\B�[B�cB�xBԀBדB��B��B��B�B�%B�=B�SB	�B	�B	�B	�B	�B	�B	�B	%B	8B	!EB	"NB	$ZB	+�B	.�B	/�B	0�B	1�B	1�B	2�B	2�B	3�B	4�B	:�B	=�B	>�B	>�B	?�B	AB	DB	F"B	G&B	I4B	J;B	MLB	P]B	QeB	RiB	SpB	UxB	W�B	[�B	]�B	a�B	b�B	d�B	d�B	e�B	mB	pB	r$B	u7B	v=B	wGB	wGB	yRB	zUB	|bB	wB	uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�!B	�"B	�8B	�NB	�\B	�]B	�bB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�"B	�)B	�.B	�@B	�HB	�FB	�IB	�PB	�KB	�TB	�SB	�TB	�^B	�nB	�kB	�wB	�}B	؄B	؃B	يB	ۗB	ەB	ݥB	ݡB	ަB	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	� B	�-B	�+B	�&B	�(B	�)B	�)B	�0B	�BB	�@B	�@G�O�B	��B
	sB
�B
�B
#[B
'�B
4�B
9BB
>}B
B�B
E�B
L�B
QSB
W'B
[�B
^!B
ctB
h�B
m�B
p�B
sn111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.02 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9997(+/-0), vertically averaged dS =-0.012(+/-0.002) in PSS-78.                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Map scales: x=6,3; y=2,1. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                            201906040940302019060409403020190604094030  AO  ARCAADJP                                                                    20171101150523    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20171101150523  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20171101150523  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190604094030  IP                  G�O�G�O�G�O�                