CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-11-21T04:15:20Z creation      
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
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20181121041520  20190604095259  5903740 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  4055                            2C  D   APEX                            5374                            041511                          846 @׷��y�1   @׷��'��@;cS����cM�-V1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @,��@y��@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B/��B8  B?��BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C �C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dtl�Dy�D� D�@�D��fD�ÅD��D�B�D�t�D��)D��D�MD�p�D���D���D�G�Dښ=D���D��D�:�D�n�D��\111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @>�R@�@���Az�A$z�ADz�Adz�A�=qA�=qA�=qA�=qA�=qA�=qA�=qA�=qB�B	�B�B�B!�B)�B0�RB9�B@�RBI�BQ�BY�Ba�Bi�Bq�By�B��\B��\B��\B��\B��\B��\B��\B��\B��\B��\B��\B��\B��\B��\B��\B��\B��\Bď\Bȏ\B̏\BЏ\Bԏ\B؏\B܏\B��\B�\B�\B�\B��\B�\B��\B��\C G�CG�CG�CG�CG�C
G�CG�CG�CG�CG�CG�CG�CG�CG�CG�CG�C aHC"G�C$G�C&G�C(G�C*G�C,G�C.G�C0G�C2G�C4G�C6G�C8G�C:G�C<G�C>G�C@G�CBG�CDG�CFG�CHG�CJG�CLG�CNG�CPG�CRG�CTG�CVG�CXG�CZG�C\G�C^G�C`G�CbG�CdG�CfG�ChG�CjG�ClG�CnG�CpG�CrG�CtG�CvG�CxG�CzG�C|G�C~G�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt~�Dy�D��D�I�D��\D��{D��D�K�D�}�D��D��D�VD�y�D���D��D�P�Dڣ3D���D��D�C�D�w�D��R111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�bA�oA�oA�oA�{A��A��A��A��A�
=A��mA���A�M�A�/A�&�A�bA��`A���A�?}A�A��+A��PA�S�A���A�n�A���A�E�A�`BA�ȴA�l�A��A���A�5?A�JA��uA�ZA���A�t�A��uA��A��A�%A���A�A��A�/A�^5A�^5A�&�A���A��uA�O�A�{A�O�A��TA���A�(�A�A�^5A�A���A�/A� �A�v�A��A�VA�1'A���A��A�G�A�VA�ZA���A�ȴA��7A�dZA���A�&�A~��A|��A|  Az�Ay��Ax�AwG�AwVAv�\Au?}Atz�Atn�As�Ar��Aq&�Apv�Ao;dAmC�Aln�Ak%Ai�Ah�RAh�Agt�Af�!Ae�
AeC�Ad  AcXAb�DAa��Aa��Aa�7A`n�A_x�A^~�A^bA\��A[�FAZ5?AX�yAV1'ATĜAS�-ARĜAQAQK�AP�AP�jAP^5AO��ANz�ANJAM�AL�+AK&�AJ1'AIXAG�TAEƨAE�AD^5AC��ABȴAA��AA&�A@�+A?t�A?+A>�yA>^5A=;dA<�jA<n�A;�A:��A:Q�A9��A8��A8�A7��A7oA6ffA5��A4�/A3��A2�\A1&�A0 �A/
=A.(�A-�A-;dA+�TA+dZA*�HA*�\A)K�A(E�A'�7A&�A&�DA%�A%�A%33A$��A$E�A#�7A"�`A"VA!|�A!VA ~�AhsA�A��A��A �A��A�A�A��A��A��AK�A�A$�A�7A��A�\A�A��A �At�A��AAS�AȴA�+A=qA�A��AAS�A
�DA	�7A	%A�A�`A9XA�A��A��A�DA-AO�A��A �D@�33@�$�@�V@��D@�o@��F@���@���@�33@�5?@�/@�~�@��^@�Z@ꟾ@�h@�Z@���@��@�K�@�n�@�@��H@�-@�`B@���@ܣ�@���@��@�=q@��@׍P@�E�@��@ՙ�@�%@ԃ@�1@ҏ\@��@�7L@϶F@�J@���@��m@��@�~�@��@�`B@�|�@�-@őh@ě�@�;d@�~�@�E�@�X@�ƨ@��+@�$�@���@�7L@�j@�"�@��@�G�@�A�@��R@�%@�A�@�C�@��@��u@���@�M�@�@��-@��@�A�@�K�@�M�@��@��D@� �@��;@�+@�^5@���@�`B@�V@��j@�(�@�+@�ff@��@��T@���@��`@��@��H@��+@���@��`@���@�j@�1@�dZ@�@��@��\@�{@���@�hs@�Q�@��w@��P@�;d@�M�@��@��-@�7L@��j@��@�9X@��;@��F@�l�@�;d@�n�@�^5@���@���@���@��+@�E�@���@���@�X@�/@���@��@��@��@��9@�r�@�Q�@�9X@���@��F@��F@���@�;d@���@��!@�~�@�^5@��@��h@�X@���@�z�@�1'@��;@�dZ@�S�@�K�@�;d@��@��!@�~�@�n�@��@�J@��-@�X@�%@��@�r�@�9X@��@�  @��m@���@�+@���@�M�@��@��@���@��-@��h@��7@��7@�/@���@���@��D@� �@��w@��m@���@��w@�K�@���@�ff@�-@��-@��h@�`B@�O�@���@��/@�A�@�9X@�bN@�;@�b@�bN@���@���@��@���@���@��@��u@�bN@�9X@�(�@�1@�w@�@|�@�@~�+@~E�@~@}��@}�@}?}@}/@}V@|�@|�j@|Z@{�m@{ƨ@{S�@{@z-@y�@y��@y�@w��@ux�@l�@f�M@`��@Zz@TS�@M@Fl�@>��@:�A@34�@-#�@'�@#�A@8@��@�3@�x@ƨ@��@�6111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A�bA�oA�oA�oA�{A��A��A��A��A�
=A��mA���A�M�A�/A�&�A�bA��`A���A�?}A�A��+A��PA�S�A���A�n�A���A�E�A�`BA�ȴA�l�A��A���A�5?A�JA��uA�ZA���A�t�A��uA��A��A�%A���A�A��A�/A�^5A�^5A�&�A���A��uA�O�A�{A�O�A��TA���A�(�A�A�^5A�A���A�/A� �A�v�A��A�VA�1'A���A��A�G�A�VA�ZA���A�ȴA��7A�dZA���A�&�A~��A|��A|  Az�Ay��Ax�AwG�AwVAv�\Au?}Atz�Atn�As�Ar��Aq&�Apv�Ao;dAmC�Aln�Ak%Ai�Ah�RAh�Agt�Af�!Ae�
AeC�Ad  AcXAb�DAa��Aa��Aa�7A`n�A_x�A^~�A^bA\��A[�FAZ5?AX�yAV1'ATĜAS�-ARĜAQAQK�AP�AP�jAP^5AO��ANz�ANJAM�AL�+AK&�AJ1'AIXAG�TAEƨAE�AD^5AC��ABȴAA��AA&�A@�+A?t�A?+A>�yA>^5A=;dA<�jA<n�A;�A:��A:Q�A9��A8��A8�A7��A7oA6ffA5��A4�/A3��A2�\A1&�A0 �A/
=A.(�A-�A-;dA+�TA+dZA*�HA*�\A)K�A(E�A'�7A&�A&�DA%�A%�A%33A$��A$E�A#�7A"�`A"VA!|�A!VA ~�AhsA�A��A��A �A��A�A�A��A��A��AK�A�A$�A�7A��A�\A�A��A �At�A��AAS�AȴA�+A=qA�A��AAS�A
�DA	�7A	%A�A�`A9XA�A��A��A�DA-AO�A��A �D@�33@�$�@�V@��D@�o@��F@���@���@�33@�5?@�/@�~�@��^@�Z@ꟾ@�h@�Z@���@��@�K�@�n�@�@��H@�-@�`B@���@ܣ�@���@��@�=q@��@׍P@�E�@��@ՙ�@�%@ԃ@�1@ҏ\@��@�7L@϶F@�J@���@��m@��@�~�@��@�`B@�|�@�-@őh@ě�@�;d@�~�@�E�@�X@�ƨ@��+@�$�@���@�7L@�j@�"�@��@�G�@�A�@��R@�%@�A�@�C�@��@��u@���@�M�@�@��-@��@�A�@�K�@�M�@��@��D@� �@��;@�+@�^5@���@�`B@�V@��j@�(�@�+@�ff@��@��T@���@��`@��@��H@��+@���@��`@���@�j@�1@�dZ@�@��@��\@�{@���@�hs@�Q�@��w@��P@�;d@�M�@��@��-@�7L@��j@��@�9X@��;@��F@�l�@�;d@�n�@�^5@���@���@���@��+@�E�@���@���@�X@�/@���@��@��@��@��9@�r�@�Q�@�9X@���@��F@��F@���@�;d@���@��!@�~�@�^5@��@��h@�X@���@�z�@�1'@��;@�dZ@�S�@�K�@�;d@��@��!@�~�@�n�@��@�J@��-@�X@�%@��@�r�@�9X@��@�  @��m@���@�+@���@�M�@��@��@���@��-@��h@��7@��7@�/@���@���@��D@� �@��w@��m@���@��w@�K�@���@�ff@�-@��-@��h@�`B@�O�@���@��/@�A�@�9X@�bN@�;@�b@�bN@���@���@��@���@���@��@��u@�bN@�9X@�(�@�1@�w@�@|�@�@~�+@~E�@~@}��@}�@}?}@}/@}V@|�@|�j@|Z@{�m@{ƨ@{S�@{@z-@y�@y��@y�G�O�@ux�@l�@f�M@`��@Zz@TS�@M@Fl�@>��@:�A@34�@-#�@'�@#�A@8@��@�3@�x@ƨ@��@�6111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBD�BD�BD�BD�BD�BD�BE�BE�BD�BG�BJ�BM�BM�BK�BJ�BH�BE�BB�B=qB8RB+BPB��Bn�BB��B�DB�7Bu�Bk�BaHB^5B[#BZBT�BQ�BP�BN�BH�B9XB�BVB��B�B�ZB�HB�B��BĜB�dB��B�{B�Bv�Bn�B_;BdZB`BB_;BW
BS�BI�B9XB.B#�B�B�BuB+B
��B
��B
�B
�NB
��B
�wB
�B
��B
��B
�DB
|�B
u�B
l�B
bNB
ZB
P�B
M�B
H�B
?}B
8RB
7LB
1'B
%�B
�B
bB
B	��B	�B	�HB	�/B	�B	��B	��B	��B	ŢB	��B	�RB	�3B	�B	��B	��B	��B	��B	��B	��B	�hB	�=B	~�B	p�B	dZB	Q�B	G�B	?}B	:^B	5?B	:^B	=qB	J�B	L�B	F�B	A�B	@�B	=qB	9XB	0!B	)�B	 �B	uB	B	B	B��B��B�B�B�B�mB�fB�fB�`B�BB�)B�)B�B�
B��B��B��B��B��BɺBǮBĜB��B�wB�RB�9B�'B�B�B�B��B��B��B��B��B��B��B��B��B�{B�oB�hB�\B�VB�JB�=B�+B�B�B~�B{�Bw�Bt�Bp�Bm�BjBhsBe`BaHB_;B]/B[#BZBXBVBS�BR�BO�BL�BJ�BH�BG�BD�BB�B@�B?}B>wB<jB:^B8RB6FB49B2-B/B-B+B(�B'�B&�B%�B$�B#�B!�B�B�B�B�B�B�B�B{BbB\BhBhBbBJB1B%BBBBBB��B��B��B��B��B��B��B��B��B��B��B  BBBBBBBBBBBB  BB  B��B��B��BBBB+B1B1BDBJBJBJBVBbBbBbBbBbBoB�B�B�B�B!�B"�B$�B&�B(�B1'B49B5?B5?B5?B5?B6FB7LB:^B<jB=qB=qB@�BC�BE�BF�BG�BH�BJ�BN�BQ�BS�BS�BT�BXB[#B`BBaHBe`BiyBiyBk�Bl�Bp�Bs�Bu�Bw�Bz�B{�B}�B�B�+B�1B�VB�uB�uB�uB�{B��B��B��B��B��B��B��B��B�B�3B�?B�XB�^B�jB�wB�wB�}B�}B��BBĜBĜBĜBɺB��B��B��B��B��B��B��B�B�B�#B�)B�BB�NB�ZB�B�B�B�B��B��B��B��B��B��B��B��B	B	B	B	1B	JB	\B	hB	oB	uB	{B	{B	�B	�B	�B	 �B	"�B	#�B	'�B	)�B	+B	-B	/B	0!B	5?B	8RB	9XB	;dB	<jB	>wB	?}B	B�B	C�B	E�B	E�B	F�B	I�B	K�B	N�B	N�B	O�B	T�B	W
B	YB	[#B	\)B	_;B	e`B	jB	l�B	p�B	r�B	r�B	r�B	s�B	u�B	v�B	v�B	w�B	x�B	y�B	z�B	{�B	|�B	|�B	}�B	~�B	�B	�B	�B	�B	�%B	�+B	�1B	�=B	�DB	�JB	�VB	�\B	�bB	�uB	�uB	��B	��B	�*B	��B	��B	��B
_B
{B
 �B
*�B
0oB
9	B
?cB
F�B
K)B
Q B
[�B
]�B
bB
g�B
m�B
r�111111111111111111111111441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  BD�BD�BD�BD�BD�BD�BE�BE�BD�BG�BJ�BM�BM�BK�BJ�BH�BE�BBwB=ZB8;B*�B8B��BnG�O�G�O�B�*B�Bu�BkiBa-B^B[	BZBT�BQ�BP�BN�BH�B9>B�B9B��B�fB�=B�-B�BʥBāB�IB��B�`B��Bv�Bn~B_Bd@B`$B_!BV�BS�BI�B99B-�B#�B�BvBVBB
��B
��B
�nB
�2B
��B
�]B
��B
��B
��B
�'B
|�B
u�B
lpB
b0B
ZB
P�B
M�B
H�B
?^B
85B
7.B
1
B
%�B
qB
DB
�B	��B	�pB	�+B	�B	��B	��B	��B	̱B	ŅB	�fB	�4B	�B	��B	��B	��B	��B	��B	��B	�cB	�HB	�B	~�B	p�B	d>B	Q�B	G�B	?`B	:BB	5"B	:AB	=SB	J�B	L�B	F�B	AkB	@fB	=RB	98B	0B	)�B	 �B	YB	�B	�B	 �B��B��B��B�lB�mB�NB�FB�GB�@B�$B�	B�B��B��B��B��BϽBͳB˨BɜBǐB�{B�cB�VB�3B�B�B��B��B��B��B��B��B��B��B��B�B�qB�gB�]B�MB�GB�<B�3B�)B�B�B��B��B~�B{�Bw�Bt�Bp�BmoBj`BhQBe@Ba*B_B]B[BY�BW�BU�BS�BR�BO�BL�BJ�BH�BG�BDzBBmB@aB?[B>VB<HB:?B81B6&B4B2	B.�B,�B*�B(�B'�B&�B%�B$�B#�B!�B�B�BwBlBiBlBkBWB@B9BGBGB?B)BBB�B�B�B�B �B��B��B��B��B��B��B��B��B��B��B��B��B �B �B�B�B�B�B�B�B �B �B �B��B �B��B��B��B��B �B�B�BBBB"B'B%B%B2B?B=B>B?B;BLB]BkBvB�B!�B"�B$�B&�B(�B1B4B5B5B5B5B6$B7)B:;B<EB=LB=NB@]BCsBE|BF�BG�BH�BJ�BN�BQ�BS�BS�BT�BW�B[ B`Ba#Be<BiWBiTBkbBlgBp�Bs�Bu�Bw�Bz�B{�B}�B��B�B�B�2B�PB�OB�PB�UB�iB�uB�{B��B��B��B��B��B��B�B�B�4B�8B�FB�TB�RB�YB�ZB�aB�jB�wB�yB�wBɔBˣB̨BϻB��BпB��B��B��B��B��B�B�B�*B�4B�ZB�dB�tB�B��B��B��B��B��B��B��B��B	�B	 �B	�B	B	$B	8B	DB	KB	PB	VB	WB	`B	vB	�B	 �B	"�B	#�B	'�B	)�B	*�B	,�B	.�B	/�B	5B	8+B	92B	;>B	<CB	>QB	?XB	BjB	CrB	E{B	E{B	F�B	I�B	K�B	N�B	N�B	O�B	T�B	V�B	X�B	Z�B	\B	_B	e9B	j[B	lhB	pB	r�B	r�B	r�B	s�B	u�B	v�B	v�B	w�B	x�B	y�B	z�B	{�B	|�B	|�B	}�B	~�B	��B	��B	��B	��B	��B	�B	�B	�B	�"B	�&B	�1B	�8B	�;B	�QB	�OG�O�B	�vB	�B	׹B	�B	��B
;B
WB
 �B
*rB
0KB
8�B
??B
F�B
KB
P�B
[�B
]uB
a�B
g�B
m�B
r�111111111111111111111111441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.28 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             201906040952592019060409525920190604095259  AO  ARCAADJP                                                                    20181121041520    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181121041520  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181121041520  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190604095259  IP                  G�O�G�O�G�O�                