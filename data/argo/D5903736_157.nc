CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-11-21T04:11:52Z creation      
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
resolution        :�o     �  qt   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yh   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {h   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �\   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �\   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �P   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �    HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �$   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �(   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �,Argo profile    3.1 1.2 19500101000000  20181121041152  20190604094022  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  4051                            2C  D   APEX                            5368                            041511                          846 @�Ï��Y�1   @�Ð=�7@3,I�^5?�dXQ��1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @���@���A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dtl�Dy��D��D�9�D���D�ФD�qD�HRD�j�D���D��HD�>D���D��{D��qD�2=Dڞ�D�˅D�)D�I�D�x�D��H11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @���@�(�@�\)A�A?�A_�A�A��
A��
A��
A��
A��
A��
A��
A��
B�B�B�B�B'�B/�B7�B?�BG�BO�BW�B_�Bg�Bo�Bw�B�B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B�(�B���B���B���B���B���B���B���B���B���B���B���B���B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��qC��qC��qC��qC��C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC�
>C�
>C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC�
>C�
>C��qC��qC��qC��qD ~�D ��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D	~�D	��D
~�D
��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D ~�D ��D!~�D!��D"~�D"��D#~�D#��D$~�D$��D%~�D%��D&~�D&��D'~�D'��D(~�D(��D)~�D)��D*~�D*��D+~�D+��D,~�D,��D-~�D-��D.~�D.��D/~�D/��D0~�D0��D1~�D1��D2~�D2��D3~�D3��D4~�D4��D5~�D5��D6~�D6��D7~�D7��D8~�D8��D9~�D9��D:~�D:��D;~�D;��D<~�D<��D=~�D=��D>~�D>��D?~�D?��D@~�D@��DA~�DA��DB~�DB��DC~�DC��DD~�DD��DE~�DE��DF~�DF��DG~�DG��DH~�DH��DI~�DI��DJ~�DJ��DK~�DK��DL~�DL��DM~�DM��DN~�DN��DO~�DO��DP~�DP��DQ~�DQ��DR~�DR��DS~�DS��DT~�DT��DU~�DU��DV~�DV��DW~�DW��DX~�DX��DY~�DY��DZ~�DZ��D[~�D[��D\~�D\��D]~�D]��D^~�D^��D_~�D_��D`~�D`��Da~�Da��Db~�Db��Dc~�Dc��Dd~�Dd��De~�De��Df~�Df��Dg~�Dg��Dh~�Dh��Di~�Di��Dj~�Dj��Dk~�Dk��Dl~�Dl��Dm~�Dm��Dn~�Dn��Do~�Do��Dp~�Dp��Dq~�Dq��Dr~�Dr��Ds~�Ds��Dtk�Dy��D��D�9HD��=D�� D��D�G�D�j=D��3D���D�=pD���D���D���D�1�DڞD���D��D�H�D�xRD�Ȥ11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��A��A���A�`BA��A�  A��;A��A���A���A���A���A���A���A���A�ƨA���Aߺ^A߸RA߶FA߶FA߶FA߬A�dZA�n�A���A���A���A�t�A�x�A֟�A���A�ƨA�ffA�M�A�AсA�oA��AϼjA�O�A��A��#A�ȴA�r�A���A���Aʗ�A�{A�ȴA�x�A�VA�-Aƙ�A�l�Aŉ7A���A�dZA��AA��A�1'A�hsA�1'A��A�XA�(�A���A��HA�ƨA���A��
A��A�?}A��A�&�A��;A�ƨA���A��#A��A�C�A�l�A�M�A�G�A�oA���A��;A��A�33A���A�=qA��DA�\)A���A���A��PA���A��A��hA�jA�dZA���A�bNA�=qA��A�ĜA���A� �A���A���A��A���A�%A��RA��A�M�A��+A��/A��A� �A��A��A�$�A�x�A���A��A~=qA}��A|�uAz�+Aw�mAvVAu7LAtbNAr^5Ap  Ao?}Al�+Ah�Ag�^AfA�Ae�Ad=qAc/Abn�Aa��A`n�A^�A]�PA\��AZ��AZ(�AX1AT�jAP��AMXAK��AJ�`AI7LACl�A@�/A?K�A=hsA<n�A9/A8z�A7?}A57LA4ĜA3��A2�uA1�#A0^5A/��A/�PA/�A.��A, �A)\)A(bNA'A&5?A#�hA"��A"I�A!��A!S�A ��A �A?}A9XA��A�/A�wA33Ar�A��A\)A��AZA��A��A�HA|�A�A�\A��A�A
{A	hsA	+A\)A�yA�\A��A�HAZA5?Ap�AjA��A�@��P@��-@��!@��@��@�I�@��@��\@��@�M�@��@��@���@�`B@�Q�@�v�@�C�@�@�t�@�C�@�o@��@�j@���@�-@�;d@�+@�J@߅@��@�p�@���@ץ�@�{@��
@�V@���@Ώ\@��y@�hs@ȣ�@�+@�O�@�G�@ēu@�9X@��@��
@Ý�@�\)@��@��@���@§�@�$�@��@��j@�1@�~�@���@���@���@�@���@��@�ff@��D@�ƨ@��
@�-@��P@�t�@�|�@�|�@�M�@��@�`B@���@��;@�l�@��H@�n�@�p�@��/@�\)@�o@���@�@�x�@���@���@�33@��!@���@���@��9@��u@�  @�t�@��H@���@�v�@�E�@�$�@��^@�O�@�z�@�1@��;@��@�"�@���@�M�@��T@�7L@�%@��`@�Ĝ@���@���@��@�C�@��@���@��\@�5?@�J@�@�x�@�/@���@�bN@�Z@��@�ƨ@���@��@�33@���@�~�@�M�@�$�@��@���@�&�@���@���@���@�(�@��
@�ƨ@���@�dZ@�K�@�;d@�+@�o@���@��y@���@�E�@��@�{@��@��-@��h@�O�@�7L@�%@���@�I�@�(�@���@��@��y@���@���@��+@�n�@�V@��@���@��^@���@�x�@�/@��9@�z�@�Z@�(�@�  @�\)@�;d@�;d@�
=@��R@�o@�;d@�t�@�S�@��@�=q@�V@�M�@�^5@���@��j@��@�Q�@�A�@��m@�@���@�M�@���@�x�@�G�@�p�@�V@���@�j@�I�@�(�@�9X@� �@���@��F@�1'@� �@��w@���@��P@�@�v�@��@�M�@���@�~�@�n�@�5?@�@���@�X@��@��/@���@�Q�@�(�@���@��w@���@�dZ@�;d@�33@�o@��!@���@���@���@��+@�^5@�=q@�$�@�$�@��@��>@{U�@sRT@j�F@_�*@XK^@QY�@J�H@C i@<Ft@6�6@2+k@,��@'�@@"�@�z@u�@a@|�@bN@�/11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   A��A��A���A�`BA��A�  A��;A��A���A���A���A���A���A���A���A�ƨA���Aߺ^A߸RA߶FA߶FA߶FA߬A�dZA�n�A���A���A���A�t�A�x�A֟�A���A�ƨA�ffA�M�A�AсA�oA��AϼjA�O�A��A��#A�ȴA�r�A���A���Aʗ�A�{A�ȴA�x�A�VA�-Aƙ�A�l�Aŉ7A���A�dZA��AA��A�1'A�hsA�1'A��A�XA�(�A���A��HA�ƨA���A��
A��A�?}A��A�&�A��;A�ƨA���A��#A��A�C�A�l�A�M�A�G�A�oA���A��;A��A�33A���A�=qA��DA�\)A���A���A��PA���A��A��hA�jA�dZA���A�bNA�=qA��A�ĜA���A� �A���A���A��A���A�%A��RA��A�M�A��+A��/A��A� �A��A��A�$�A�x�A���A��A~=qA}��A|�uAz�+Aw�mAvVAu7LAtbNAr^5Ap  Ao?}Al�+Ah�Ag�^AfA�Ae�Ad=qAc/Abn�Aa��A`n�A^�A]�PA\��AZ��AZ(�AX1AT�jAP��AMXAK��AJ�`AI7LACl�A@�/A?K�A=hsA<n�A9/A8z�A7?}A57LA4ĜA3��A2�uA1�#A0^5A/��A/�PA/�A.��A, �A)\)A(bNA'A&5?A#�hA"��A"I�A!��A!S�A ��A �A?}A9XA��A�/A�wA33Ar�A��A\)A��AZA��A��A�HA|�A�A�\A��A�A
{A	hsA	+A\)A�yA�\A��A�HAZA5?Ap�AjA��A�@��P@��-@��!@��@��@�I�@��@��\@��@�M�@��@��@���@�`B@�Q�@�v�@�C�@�@�t�@�C�@�o@��@�j@���@�-@�;d@�+@�J@߅@��@�p�@���@ץ�@�{@��
@�V@���@Ώ\@��y@�hs@ȣ�@�+@�O�@�G�@ēu@�9X@��@��
@Ý�@�\)@��@��@���@§�@�$�@��@��j@�1@�~�@���@���@���@�@���@��@�ff@��D@�ƨ@��
@�-@��P@�t�@�|�@�|�@�M�@��@�`B@���@��;@�l�@��H@�n�@�p�@��/@�\)@�o@���@�@�x�@���@���@�33@��!@���@���@��9@��u@�  @�t�@��H@���@�v�@�E�@�$�@��^@�O�@�z�@�1@��;@��@�"�@���@�M�@��T@�7L@�%@��`@�Ĝ@���@���@��@�C�@��@���@��\@�5?@�J@�@�x�@�/@���@�bN@�Z@��@�ƨ@���@��@�33@���@�~�@�M�@�$�@��@���@�&�@���@���@���@�(�@��
@�ƨ@���@�dZ@�K�@�;d@�+@�o@���@��y@���@�E�@��@�{@��@��-@��h@�O�@�7L@�%@���@�I�@�(�@���@��@��y@���@���@��+@�n�@�V@��@���@��^@���@�x�@�/@��9@�z�@�Z@�(�@�  @�\)@�;d@�;d@�
=@��R@�o@�;d@�t�@�S�@��@�=q@�V@�M�@�^5@���@��j@��@�Q�@�A�@��m@�@���@�M�@���@�x�@�G�@�p�@�V@���@�j@�I�@�(�@�9X@� �@���@��F@�1'@� �@��w@���@��P@�@�v�@��@�M�@���@�~�@�n�@�5?@�@���@�X@��@��/@���@�Q�@�(�@���@��w@���@�dZ@�;d@�33@�o@��!@���@���@���@��+@�^5@�=q@�$�@�$�G�O�@��>@{U�@sRT@j�F@_�*@XK^@QY�@J�H@C i@<Ft@6�6@2+k@,��@'�@@"�@�z@u�@a@|�@bN@�/11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBA�BA�B@�B>wB=qB=qB=qB=qB=qB=qB=qB=qB=qB=qB=qB=qB=qB=qB=qB=qB>wB=qB=qB>wBD�BP�Be`Bk�BiyBbNB]/BQ�BE�B8RB2-B49B8RBD�Bm�B�B�XB�B�)B��B�qB�B�B �B.B/B/B<jB@�BYBo�B�uB��B��B��B�}B��B��B��B��B�B��BÖBÖB��B�/B�)B��B�B�+B{�Bs�Bl�BgmBp�Bm�BiyBbNB_;BiyBt�B|�B�Bz�BcTBL�BE�B?}B5?B'�B"�B"�B�B\BB��B�sB��BB�}B�wB�^B�B��B��B~�Bq�BT�BD�B>wB(�B�BB
�B
��B
��B
�RB
�FB
�9B
��B
��B
�uB
�B
l�B
ffB
]/B
K�B
=qB
49B
/B
'�B
�B
1B
  B	�B	�B	��B	ƨB	�wB	�RB	�3B	�B	��B	��B	�uB	�7B	�B	w�B	o�B	aHB	J�B	6FB	(�B	!�B	�B	VB�B�yB�TB�#B�B��B��BǮBB��B�qB�dB�RB�3B�!B�B�B��B��B��B�uB�bB�DB�%B�B�B�B~�B}�B|�By�Bv�Bu�Bu�Bu�Bt�Bs�Br�Bq�Bs�Bv�Bw�Bw�Bv�Bu�Bu�Bt�Br�Br�Bs�Bt�Bv�B�%B�1B�7B�DB�JB�DB�=B�+B�B�B�B�Bx�Bo�BjBiyBiyBhsBgmBe`BcTB_;BZB]/B_;BhsB�B�hB�{B��B��B��B��B��B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�?B�?B�^B�jB�qB�wB��BBÖBBBBŢB��B��B��B��B�/B�/B�5B�;B�;B�NB�B	B	+B	1B	B	B	B		7B	\B	JB	JB	uB	{B	uB	uB	uB	oB	hB	oB	�B	�B	�B	 �B	$�B	%�B	%�B	&�B	+B	/B	5?B	6FB	6FB	9XB	<jB	@�B	B�B	C�B	D�B	E�B	H�B	J�B	P�B	S�B	T�B	XB	ZB	]/B	_;B	bNB	gmB	hsB	iyB	jB	k�B	p�B	r�B	t�B	u�B	x�B	x�B	z�B	z�B	|�B	~�B	� B	�B	�B	�B	�+B	�7B	�=B	�DB	�PB	�bB	�oB	�uB	�uB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�!B	�-B	�3B	�9B	�?B	�LB	�LB	�RB	�dB	�jB	�wB	�}B	�}B	�}B	�}B	��B	��B	��B	��B	B	ÖB	ŢB	ƨB	ƨB	ǮB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�)B	�)B	�#B	�)B	�/B	�;B	�BB	�;B	�BB	�BB	�TB	�TB	�`B	�mB	�mB	�mB	�sB	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
%B
%B
+B
1B
	7B
	7B

=B
(B
�B
'�B
2GB
7�B
>(B
DB
K^B
QNB
U�B
Z�B
^B
b�B
gRB
lWB
p�B
t�B
yrB
|�B
�iB
��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B?4B?4B>-B< B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B;B< B;B;B<BBHBN�BcBi.Bg!B_�BZ�BO�BCNB5�B/�B1�B5�BBDBk8B��B��BּB��BИB�B�ZBGBhB+�B,�B,�B:B>*BV�BmEB�B�&B�gB��B� BώB̈́BΉB�tBղB�sB�9B�8B�xB��B��B͉B��B��By�Bq\Bj0BeBnEBk9BgB_�B\�Bg"BreBz�B��Bx�B`�BJuBCDB=(B2�B%�B yB yB]BB�B��B�B�|B�5B�+B�B�B��B��B�KB|�BoXBR�BBIB<'B&�BVB
��B
�DB
ΕB
�.B
�B
��B
��B
��B
�gB
�!B
�B
j8B
dB
Z�B
IzB
;#B
1�B
,�B
%�B
LB
�B	��B	�CB	��B	͕B	�^B	�,B	�B	��B	��B	��B	�nB	�(B	��B	�B	u�B	mVB	^�B	HzB	3�B	&�B	�B	XB	B�nB�2B�B��BӿBϤBɀB�dB�OB�;B�*B�B�B��B��B��B��B��B��B�IB�2B�B�B��B��B�B~�B|�B{�Bz�Bw�Bt�Bs�Bs|BsBryBquBpkBogBqoBt�Bu�Bu�Bt�Bs�Bs�Br}BpiBpoBqrBrwBt�B��B��B��B��B�B� B��B��B��B��B��B~�Bv�BmZBh;Bg7Bg8Bf/Be)BcBaB\�BW�BZ�B\�Bf0B~�B�(B�9B�hB��B��B��B��B��B��B��B��B��B��B�oB�zB�wB�}B�tB�nB�oB�sB�nB��B��B��B��B� B��B�B�)B�.B�4B�HB�KB�SB�OB�MB�GB�[BʊBɃB˓BҸB��B��B��B��B��B�B�;B��B	�B	�B	�B��B	 �B	�B	B	
B	
B	5B	7B	+B	3B	/B	*B	$B	)B	@B	hB	qB	~B	"�B	#�B	#�B	$�B	(�B	,�B	2�B	4B	4B	7B	:%B	>?B	@JB	ARB	BXB	C\B	FoB	H}B	N�B	Q�B	R�B	U�B	W�B	Z�B	\�B	`
B	e'B	f/B	g5B	h8B	i>B	n^B	plB	rxB	sB	v�B	v�B	x�B	x�B	z�B	|�B	}�B	~�B	�B	��B	��B	��B	��B	��B	�B	�B	�&B	�-B	�.B	�1B	�AB	�TB	�hB	�jB	�nB	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�%B	�/B	�6B	�6B	�9B	�5B	�>B	�=B	�;B	�<B	�IB	�RB	�^B	�`B	�^B	�iB	�iB	�mB	�{B	ˋB	ˍB	ˏB	ϧB	ѱB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�%B	�$B	�%B	�+B	�8B	�CB	�RB	�^B	�VB	�bB	�~B	�B	�B	�B	�B	�xB	�uB	�~B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�G�O�B
�B
RB
%<B
/�B
5mB
;�B
A�B
IB
OB
S�B
X\B
[�B
`�B
eB
jB
n�B
r\B
w*B
z?B
~ B
��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.02 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9999(+/-0), vertically averaged dS =-0.002(+/-0.002) in PSS-78.                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Map scales: x=6,3; y=2,1. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                            201906040940222019060409402220190604094022  AO  ARCAADJP                                                                    20181121041152    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181121041152  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181121041152  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190604094022  IP                  G�O�G�O�G�O�                