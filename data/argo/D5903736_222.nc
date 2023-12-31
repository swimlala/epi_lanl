CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-06-14T17:02:24Z creation      
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
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20180614170224  20190604094146  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  4051                            2C  D   APEX                            5368                            041511                          846 @�jPR�1   @�jP�1V@64��E��d�fffff1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @9��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A���B ��B33B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cq�fCt  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt� Dy��D��D�9�D�w
D�ʏD��D�K�D�o\D��D���D�F�D�� D��3D�{D�A�DڎD�ȤD��fD�6�D�|)111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @6fg@|��@�ff@�ffA33A?33A_33A33A���A���A���A���Aϙ�Aߙ�A�fgB ��B  B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fC�3C�3C�3C�3C	�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C!�3C#�3C%�3C'�3C)�3C+�3C-�3C/�3C1�3C3�3C5�3C7�3C9�3C;�3C=�3C?�3CA�3CC�3CE�3CG�3CI�3CK�3CM�3CO�3CQ�3CS�3CU�3CW�3CY�3C[�3C]�3C_�3Ca�3Cc�3Ce�3Cg�3Ci�3Ck�3Cm�3Co�3CqٙCs�3Cu�3Cw�3Cy�3C{�3C}�3C�3C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D |�D ��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D	|�D	��D
|�D
��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D |�D ��D!|�D!��D"|�D"��D#|�D#��D$|�D$��D%|�D%��D&|�D&��D'|�D'��D(|�D(��D)|�D)��D*|�D*��D+|�D+��D,|�D,��D-|�D-��D.|�D.��D/|�D/��D0|�D0��D1|�D1��D2|�D2��D3|�D3��D4|�D4��D5|�D5��D6|�D6��D7|�D7��D8|�D8��D9|�D9��D:|�D:��D;|�D;��D<|�D<��D=|�D=��D>|�D>��D?|�D?��D@|�D@��DA|�DA��DB|�DB��DC|�DC��DD|�DD��DE|�DE��DF|�DF��DG|�DG��DH|�DH��DI|�DI��DJ|�DJ��DK|�DK��DL|�DL��DM|�DM��DN|�DN��DO|�DO��DP|�DP��DQ|�DQ��DR|�DR��DS|�DS��DT|�DT��DU|�DU��DV|�DV��DW|�DW��DX|�DX��DY|�DY��DZ|�DZ��D[|�D[��D\|�D\��D]|�D]��D^|�D^��D_|�D_��D`|�D`��Da|�Da��Db|�Db��Dc|�Dc��Dd|�Dd��De|�De��Df|�Df��Dg|�Dg��Dh|�Dh��Di|�Di��Dj|�Dj��Dk|�Dk��Dl|�Dl��Dm|�Dm��Dn|�Dn��Do|�Do��Dp|�Dp��Dq|�Dq��Dr|�Dr��Ds|�Ds��Dt|�Dt��Dy��D�)D�8RD�upD���D�D�I�D�m�D��zD��RD�ED��fD�ٙD��D�@ DڌzD��
D���D�5D�z�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�M�A�E�A��A��
A�l�A���A�I�Aҝ�A�r�A�\)A�VA�ƨAѩ�A�VAоwAϡ�A�l�A�K�A��A�-A�ffA��yA�+A���A�7LA�|�Aº^A���A�~�A�9XA��/A�(�A��7A�5?A��/A���A�x�A��A�A�9XA�A�VA�5?A�5?A�&�A��
A�A���A�G�A��wA�JA�K�A���A�A�C�A��TA��7A�?}A��A�oA�9XA�1'A��A�VA��jA�v�A�O�A��RA�7LA�I�A�{A�hsA��yA�~�A�bA�z�A�ȴA�~�A�=qA�{A��jA�ȴA��jA���A�%A��^A�&�A���A���A�&�A���A�1'A���A��hA�&�A�n�A�?}A��^A��A�;dA��9A�t�A��9A�O�A�$�A��A��FA�E�A�n�A�?}A�I�A
=A|ffAz�`Ay�wAx�9Aw�PAv��AuƨAs33Aq�Ao�FAo|�Ao
=Ann�AkhsAi�TAioAh{AfJAc�AbbNAa�7A`�DA^�!A]t�A\{AY�-AX��AW�#AV�!AU"�AS�FAR��AR��AQp�APffAOAO"�ANQ�AM�PAL(�AI�
AH��AF��AEƨAD�AC��AA\)A?�A>5?A=l�A=
=A<$�A;;dA:�DA933A8��A7��A7;dA6ȴA5�A4v�A4JA2��A1/A0A.M�A-33A,��A+��A+A)�wA(1A'dZA'C�A&�9A%�A#��A"�!A"bNA!K�AbA�A�9A�FA9XA�A�;A  A�!A�^AG�A�jA��A%A5?A��Av�A+A
bNA	�A��A�jA1A;dA��AoAv�AS�AC�A33AS�A�PA��A bN@�ȴ@�%@�l�@���@���@��T@�ȴ@��m@���@��@�=q@�;d@�`B@�t�@◍@���@�x�@ߥ�@ݩ�@�/@�9X@��@��@أ�@��@�v�@պ^@Դ9@ҟ�@��T@�(�@�|�@�"�@�ȴ@Η�@�E�@�hs@��@��/@� �@ʇ+@ɡ�@��`@ȋD@ǝ�@Ƨ�@�n�@�J@��@� �@��
@�"�@���@�/@�&�@��@��`@�z�@�(�@�|�@��@�-@�@�V@��9@��D@�z�@�j@��w@���@�7L@��u@��@�r�@�Q�@���@��P@���@�x�@��@��@�bN@��@�ƨ@��R@���@�`B@��@���@��D@�Q�@��@�  @��w@�|�@���@�V@�E�@�-@�`B@��/@�Ĝ@��@���@�(�@��
@��@��P@�33@��H@��\@�{@���@�hs@�?}@��`@�j@��F@�;d@��@��\@�{@��@�bN@��w@�C�@�~�@��@��-@�X@���@��@�|�@�C�@�
=@���@�v�@���@�p�@��-@��^@�&�@��/@���@� �@�K�@�\)@��@�  @��y@�~�@��+@���@�x�@�7L@�?}@���@�/@���@��/@��@�I�@�  @��;@���@�dZ@�@��@�ȴ@��R@�v�@�{@���@��@���@�@�@���@��@��@��T@��T@��#@��#@���@�@��-@�p�@�7L@�&�@�%@���@�j@�A�@�9X@���@��F@��@��@�l�@�\)@�33@��y@��!@�n�@�V@��@���@��@�hs@�O�@�G�@�?}@�7L@�/@�/@�&�@�%@���@���@�z�@�(�@���@�l�@�;d@�ȴ@�E�@�5?@�-@��#@�p�@�`B@���@�p�@��D@�Q�@�Z@�bN@�r�@��@��u@�A�@��@�S�@��!@�=q@�@��@��#@��^@��@�z�@�I�@��;@�|�@�l�@�dZ@�"�@��!@�E�@�-@��#@�x�@�/@��@�Q�@|��@s��@j($@c��@V	@N�B@J��@D��@>{�@8e�@4�@-�@$��@ �?@� @�<@&�@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111  A�M�A�E�A��A��
A�l�A���A�I�Aҝ�A�r�A�\)A�VA�ƨAѩ�A�VAоwAϡ�A�l�A�K�A��A�-A�ffA��yA�+A���A�7LA�|�Aº^A���A�~�A�9XA��/A�(�A��7A�5?A��/A���A�x�A��A�A�9XA�A�VA�5?A�5?A�&�A��
A�A���A�G�A��wA�JA�K�A���A�A�C�A��TA��7A�?}A��A�oA�9XA�1'A��A�VA��jA�v�A�O�A��RA�7LA�I�A�{A�hsA��yA�~�A�bA�z�A�ȴA�~�A�=qA�{A��jA�ȴA��jA���A�%A��^A�&�A���A���A�&�A���A�1'A���A��hA�&�A�n�A�?}A��^A��A�;dA��9A�t�A��9A�O�A�$�A��A��FA�E�A�n�A�?}A�I�A
=A|ffAz�`Ay�wAx�9Aw�PAv��AuƨAs33Aq�Ao�FAo|�Ao
=Ann�AkhsAi�TAioAh{AfJAc�AbbNAa�7A`�DA^�!A]t�A\{AY�-AX��AW�#AV�!AU"�AS�FAR��AR��AQp�APffAOAO"�ANQ�AM�PAL(�AI�
AH��AF��AEƨAD�AC��AA\)A?�A>5?A=l�A=
=A<$�A;;dA:�DA933A8��A7��A7;dA6ȴA5�A4v�A4JA2��A1/A0A.M�A-33A,��A+��A+A)�wA(1A'dZA'C�A&�9A%�A#��A"�!A"bNA!K�AbA�A�9A�FA9XA�A�;A  A�!A�^AG�A�jA��A%A5?A��Av�A+A
bNA	�A��A�jA1A;dA��AoAv�AS�AC�A33AS�A�PA��A bN@�ȴ@�%@�l�@���@���@��T@�ȴ@��m@���@��@�=q@�;d@�`B@�t�@◍@���@�x�@ߥ�@ݩ�@�/@�9X@��@��@أ�@��@�v�@պ^@Դ9@ҟ�@��T@�(�@�|�@�"�@�ȴ@Η�@�E�@�hs@��@��/@� �@ʇ+@ɡ�@��`@ȋD@ǝ�@Ƨ�@�n�@�J@��@� �@��
@�"�@���@�/@�&�@��@��`@�z�@�(�@�|�@��@�-@�@�V@��9@��D@�z�@�j@��w@���@�7L@��u@��@�r�@�Q�@���@��P@���@�x�@��@��@�bN@��@�ƨ@��R@���@�`B@��@���@��D@�Q�@��@�  @��w@�|�@���@�V@�E�@�-@�`B@��/@�Ĝ@��@���@�(�@��
@��@��P@�33@��H@��\@�{@���@�hs@�?}@��`@�j@��F@�;d@��@��\@�{@��@�bN@��w@�C�@�~�@��@��-@�X@���@��@�|�@�C�@�
=@���@�v�@���@�p�@��-@��^@�&�@��/@���@� �@�K�@�\)@��@�  @��y@�~�@��+@���@�x�@�7L@�?}@���@�/@���@��/@��@�I�@�  @��;@���@�dZ@�@��@�ȴ@��R@�v�@�{@���@��@���@�@�@���@��@��@��T@��T@��#@��#@���@�@��-@�p�@�7L@�&�@�%@���@�j@�A�@�9X@���@��F@��@��@�l�@�\)@�33@��y@��!@�n�@�V@��@���@��@�hs@�O�@�G�@�?}@�7L@�/@�/@�&�@�%@���@���@�z�@�(�@���@�l�@�;d@�ȴ@�E�@�5?@�-@��#@�p�@�`B@���@�p�@��D@�Q�@�Z@�bN@�r�@��@��u@�A�@��@�S�@��!@�=q@�@��@��#@��^@��@�z�@�I�@��;@�|�@�l�@�dZ@�"�@��!@�E�@�-@��#@�x�G�O�@��@�Q�@|��@s��@j($@c��@V	@N�B@J��@D��@>{�@8e�@4�@-�@$��@ �?@� @�<@&�@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�oB
�1B
�oB
�3B<jBI�BffB��B��B�5B�fB�BBbB,B@�BE�BN�B^5BiyBo�Bu�Bx�By�B|�B�7B�%B[#BcTBp�B�B�DB��B��B��B��B��B��B��B�{B�PB�7B�B�B}�Bt�B`BBW
BM�B<jB1'B-B&�B�B1BbB'�B5?B49B0!B-B&�B �B�B�BuBhBDBB��B�B�/B��B��BȴB�RB��B�=Bx�Bk�BdZB[#BD�B-BhB
��B
�B
�`B
�B
�dB
�?B
�-B
�B
��B
��B
�uB
u�B
ffB
ZB
H�B
A�B
=qB
:^B
5?B
.B
#�B
VB
B	��B	��B	��B	��B	�5B	�5B	�
B	��B	ƨB	�dB	�-B	�B	��B	�bB	�B	{�B	o�B	iyB	bNB	[#B	Q�B	M�B	J�B	G�B	C�B	B�B	D�B	A�B	B�B	?}B	;dB	33B	,B	"�B	�B	�B	oB	%B��B�B�B�B�ZB�/B�B��B��BƨBÖB��B�^B�LB�9B�B��B��B��B��B��B��B��B�uB�bB�VB�JB�=B�+B�B�B� Bz�Bs�Bn�Bl�BiyBhsBdZBbNBaHB`BB`BB_;B^5B\)B[#BZBYBVBVBT�BS�BS�BR�BQ�BP�BN�BN�BM�BO�BP�BS�BXBcTBaHBN�BK�BJ�BJ�BL�BP�BI�BG�BD�BC�BC�BE�BD�BC�BE�BE�BF�BE�BF�BH�BH�BH�BO�BW
BXBXB[#B[#B[#B^5B_;BcTBdZBe`BffBffBffBhsBiyBiyBk�Bq�Bs�Bu�Bv�Bx�Bz�B{�B{�B|�B� B�B�B�%B�=B�=B�=B�DB�JB�PB�bB�bB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�-B�3B�?B�LB�RB�XB�qBBÖBĜBƨBɺB��B��B��B��B��B�
B�B�#B�)B�TB�mB�mB�sB�yB�B�B�B�B��B��B��B	B	JB	\B	oB	�B	�B	�B	�B	�B	�B	�B	)�B	.B	33B	6FB	<jB	@�B	B�B	D�B	F�B	G�B	N�B	O�B	P�B	P�B	R�B	W
B	ZB	bNB	jB	q�B	t�B	v�B	x�B	z�B	|�B	}�B	� B	�B	�B	�%B	�B	�%B	�+B	�DB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�3B	�FB	�RB	�XB	�XB	�XB	�XB	�^B	�^B	�^B	�^B	�dB	�qB	�}B	��B	��B	ÖB	ÖB	ŢB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�)B	�)B	�/B	�/B	�/B	�;B	�HB	�HB	�ZB	�fB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
%B
%B
+B
	7B
_B
&�B
.}B
0�B
<PB
;B
A;B
IlB
K�B
TFB
YB
^OB
c�B
j�B
n�B
r�B
w�B
{�B
~�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111  B
��B
��B
��B
��B
��B
��B
�yB
�xB
�B
��B
�tB
�zB
�zB
�bB
w%B
�eB
�#B+JB8�BUAB��B��B� B�2B�yB��B�'B�B/BB4bB=�BL�BX4B^ZBd|Bg�Bh�Bk�Bw�Bt�BI�BRB_bBr�By�B�@B�wB�zB�yB�tB�aB�MB�4B|	Bw�Bs�Bp�Bl�Bc}BOBE�B<�B+3B�B�B�B]B�B�/B�B$B# B�B�B�B�BdBOBHB 5B�B��B�B�QB�B��B��B��B�4B�eBy$Bg�BZlBSCBJB3�B B cB
��B
�~B
�aB
�B
�hB
�CB
�4B
�B
�B
��B
��B
d�B
UxB
I1B
7�B
0�B
,�B
)vB
$XB
1B
�B	�vB	�*B	�B	�B	�B	��B	�_B	�[B	�5B	��B	��B	��B	�]B	�1B	��B	�B	tXB	kB	^�B	X�B	Q�B	J`B	A*B	=B	9�B	6�B	2�B	1�B	3�B	0�B	1�B	.�B	*�B	"wB	MB	B	B	�B	�B�pB�&B��B��B��BөB́B�_B�DB�B��B��B��B��B��B��B�iB�HB�0B�B�B��B��B��B��B�B}�B{�By�Bv�BsyBqoBogBjDBcB]�B[�BX�BW�BS�BQ�BP�BO�BO�BN�BM�BK�BJ�BI�BH�BEpBEoBDjBCfBCfBBaBA\B@TB>FB>IB=HB?OB@RBCfBGBR�BP�B>IB;9B:/B:1B<?B@UB9-B7B4B3B3
B5B4B3B5B5B6B5B6B8(B8(B8)B?UBF}BG�BG�BJ�BJ�BJ�BM�BN�BR�BS�BT�BU�BU�BU�BW�BX�BX�BZ�BaBc(Be7Bf9BhEBjQBkZBkXBl_BorBp{Br�Bu�By�By�By�Bz�B{�B|�B�B�B��B��B�B�B�B�B�B� B�.B�FB�UB�aB�]B�YB�_B�jB�vB��B��B��B��B��B��B��B��B�B�B�B�&B�/B�9B�?B�GB�OB�uBɅBʉB˒BҼB��B��B��B��B��B�B�B�B�+B�:B�UB�B��B��B	�B	�B	�B	�B	�B	B	B	B	[B	sB	"�B	%�B	+�B	/�B	1�B	3�B	6B	7B	>2B	?9B	@?B	@>B	BKB	FbB	IyB	Q�B	Y�B	aB	dB	fB	h-B	j8B	lDB	mIB	oUB	p[B	soB	uzB	tuB	u|B	v}B	z�B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�4B	�>B	�:B	�8B	�FB	�ZB	�^B	�dB	�pB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�'B	�,B	�1B	�GB	�OB	�WB	�^B	�gB	�cB	�aB	�dB	�fB	�fB	�iB	�pB	�sB	�tB	�wB	�xB	�}B	·B	ДB	ГB	ӤB	հB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	�B	�B	�B	�)B	�0B	�6B	�;B	�>B	�6B	�5B	�=B	�:B	�GB	�VB	�SB	�QB	�VB	�[B	�iB	�dB	�lB	�kG�O�B	�~B
�B
B
�B
  B
+�B
*YB
0}B
8�B
;$B
C�B
HZB
M�B
S2B
ZB
]�B
a�B
f�B
kB
m�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.05 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9996(+/-0.0001), vertically averaged dS =-0.016(+/-0.003) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Map scales: x=6,3; y=2,1. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                            201906040941462019060409414620190604094146  AO  ARCAADJP                                                                    20180614170224    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20180614170224  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20180614170224  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190604094146  IP                  G�O�G�O�G�O�                