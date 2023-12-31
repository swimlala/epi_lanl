CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-01-04T23:31:19Z AOML 3.0 creation; 2016-06-01T00:08:22Z UW 3.1 conversion     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7    PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7X   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8<   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8\   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~       axis      T           8`   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8h   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~            8l   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8t   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8|   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
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
resolution        :�o     �  U|   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]p   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _p   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    gd   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  id   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  qX   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yL   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {L   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �@   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �@   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �4   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �d   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �d   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �d   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �d   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20150104233119  20160531170822  5903740 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               fA   AO  4055_7112_102                   2C  D   APEX                            5374                            041511                          846 @�0����1   @�0�0�@9�hr� ��dG��v�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    fA   A   A   @�ff@���A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
�C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� DfD� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dty�Dy�fD���D�C3D�vfD��3D� D�L�D�� D��fD�3D�33D��3D�� D�	�D�6fDږfD�ٚD�3D�S3D� D��f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�z@�z�A�
A'�
AG�
Ag�
A��A��A��A��A��A��A��A��B��B	��B��B��B!��B)��B1��B9��BA��BI��BQ��BY��Ba��Bi��Bq��By��B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C }qC}qC}qC}qC}qC
�C}qC}qC}qC}qC}qC}qC}qC}qC}qC}qC }qC"}qC$}qC&}qC(}qC*}qC,}qC.}qC0}qC2}qC4}qC6}qC8}qC:}qC<}qC>}qC@}qCB}qCD}qCF}qCH}qCJ}qCL}qCN}qCP}qCR}qCT}qCV}qCX}qCZ}qC\}qC^}qC`}qCb}qCd}qCf}qCh}qCj}qCl}qCn}qCp}qCr}qCt}qCv}qCx}qCz}qC|}qC~}qC�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�K�C�K�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�1�C�>�C�>�C�>�C�>�C�>�C�1�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�D \D �\D\D�\D�D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D	\D	�\D
\D
�\D\D�\D\D��D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D�D�\D\D�\D%�D�\D\D�\D\D�\D\D�\D \D �\D!\D!�\D"\D"�\D#\D#�\D$\D$�\D%\D%�\D&\D&�\D'\D'�\D(\D(�\D)\D)�\D*\D*�\D+\D+�\D,\D,�\D-\D-�\D.\D.�\D/\D/�\D0\D0�\D1\D1�\D2\D2�\D3\D3�\D4\D4�\D5\D5�\D6\D6�\D7\D7�\D8\D8�\D9\D9�\D:\D:�\D;\D;�\D<\D<�\D=\D=�\D>\D>�\D?\D?�\D@\D@�\DA\DA�\DB\DB�\DC\DC�\DD\DD�\DE\DE�\DF\DF�\DG\DG�\DH\DH�\DI\DI�\DJ\DJ�\DK\DK�\DL\DL�\DM\DM�\DN\DN�\DO\DO�\DP\DP�\DQ\DQ�\DR\DR�\DS\DS�\DT\DT�\DU\DU�\DV\DV�\DW\DW�\DX\DX�\DY\DY�\DZ\DZ�\D[\D[�\D\\D\�\D]\D]�\D^\D^�\D_\D_�\D`\D`�\Da\Da�\Db\Db�\Dc\Dc�\Dd\Dd�\De\De�\Df\Df�\Dg\Dg�\Dh\Dh�\Di\Di�\Dj\Dj�\Dk\Dk�\Dl\Dl�\Dm\Dm�\Dn\Dn�\Do\Do�\Dp\Dp�\Dq\Dq�\Dr\Dr�\Ds\Ds�\Dt\Dt��Dy��D�{D�R�D��D���D��D�\{D���D��D�"�D�B�D���D�߮D�HD�FDڦD��HD�"�D�b�D�D��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�&�A� �A� �A�$�A�&�A�(�A�(�A�(�A�(�A�+A�+A�+A�-A�-A�/A�/A�/A�1'A�1'A�33A�33A�5?A�5?A�5?A�1'A�33A�(�A�(�A�33A�"�A��A��A�-A�7LA�5?A�1'A�(�A��A��A�$�A�$�A�+A�1'A�7LA�9XA�/A�$�A�bA�bA�VA�  A�A�A�%A�A�A�%A�1A�A�A�A�A�  A���A���A�A���A�`BA��A�M�A��A�hsA��;A���A��A�5?A�M�A��
A�bA�I�A� �A�O�A��TA��/A���A��A��PA��FA�O�A���A��RA�$�A�K�A�oA��A��/A�A��A�  A���A�$�A�-A��A��!A�l�A�(�A�9XA�9XAC�A}t�A{�^AwG�At1'Aq��AqoAp��Ap=qApAn^5Aln�Ak|�Ai��AhȴAhVAf�Ac;dAb  Aa/A`�!A`jA^��A\�DA[hsA[VAY�mAXI�AV �AS
=ARM�ARM�ARM�ARM�AR=qAQ��APQ�AO\)AL�AK�-AK|�AK�AJ^5AI�AFĜAD�\AB�yAA�hA@1A?+A>-A=;dA<��A<~�A<I�A<A;
=A9&�A8(�A6�uA5�#A5XA3l�A2n�A1l�A0��A0r�A/l�A.�`A.��A-�#A+��A*�/A(�A(VA'��A&�A&��A%K�A$�A#l�A#�A"��A"��A"�+A �A�Ar�A�A�/Ax�A�AM�A��A�`A �A�A�A��A��A(�A;dA�A��AbA��A��A�PA&�Av�A�PA�A
�A	�A5?A��A��A��A;dAA�A�AffA�-A7LA ��A $�@��R@�9X@��h@���@�@�hs@���@�@��y@���@���@�Z@��;@�33@�E�@�(�@�I�@�+@�@�7@�&�@�=q@���@�@�&�@ۥ�@ڇ+@٩�@��@��
@׮@�S�@�~�@�1'@��`@���@�o@���@̋D@�b@�dZ@�@ʸR@�~�@�5?@ɩ�@�V@Ƨ�@Õ�@���@�1@��@��y@�E�@��@���@���@�G�@��@���@�Q�@��
@�1'@�n�@��@��@��@���@�\)@��@�~�@�M�@�$�@�{@�%@��;@���@�S�@��y@��\@���@�7L@���@��@�Z@���@�l�@�5?@���@��@��H@��@��j@���@�1@���@�+@�n�@��@�I�@��P@�@���@��@�`B@��@���@��@�"�@���@���@�ff@�hs@��P@�;d@�;d@�+@�$�@���@��@���@��@���@�r�@���@�l�@���@���@��+@��#@�X@��@�1@��@�+@�
=@�
=@���@���@��\@��h@�?}@��@���@��`@�bN@�1@�  @��m@��w@�S�@�o@��@�ff@��@��#@��#@�x�@�X@���@���@�Z@� �@�b@���@��m@��F@�l�@��y@���@�V@���@���@�x�@�`B@�7L@��@��`@���@�Ĝ@��j@��j@��u@�z�@�r�@�Z@�I�@�I�@�A�@�1@�ƨ@��F@���@���@�|�@�dZ@�C�@�+@�;d@�+@�+@�K�@��w@���@�ƨ@���@�t�@�\)@�K�@�33@�@��\@�{@��^@��@��@��u@�1'@�;@��@�P@�P@�P@��@|�@�@~V@~@}��@|�@|j@|9X@{�m@{o@zn�@y�@yx�@yx�@yX@y�@x�9@xQ�@w�@wl�@w�@v�+@v$�@u�@u�@uO�@t�@t��@tz�@nff@d��@[��@V��@P��@Jn�@FE�@@1'@:�H@6@2�@,��@'�@ ��@M�@  @��@33@�9@��@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�&�A� �A� �A�$�A�&�A�(�A�(�A�(�A�(�A�+A�+A�+A�-A�-A�/A�/A�/A�1'A�1'A�33A�33A�5?A�5?A�5?A�1'A�33A�(�A�(�A�33A�"�A��A��A�-A�7LA�5?A�1'A�(�A��A��A�$�A�$�A�+A�1'A�7LA�9XA�/A�$�A�bA�bA�VA�  A�A�A�%A�A�A�%A�1A�A�A�A�A�  A���A���A�A���A�`BA��A�M�A��A�hsA��;A���A��A�5?A�M�A��
A�bA�I�A� �A�O�A��TA��/A���A��A��PA��FA�O�A���A��RA�$�A�K�A�oA��A��/A�A��A�  A���A�$�A�-A��A��!A�l�A�(�A�9XA�9XAC�A}t�A{�^AwG�At1'Aq��AqoAp��Ap=qApAn^5Aln�Ak|�Ai��AhȴAhVAf�Ac;dAb  Aa/A`�!A`jA^��A\�DA[hsA[VAY�mAXI�AV �AS
=ARM�ARM�ARM�ARM�AR=qAQ��APQ�AO\)AL�AK�-AK|�AK�AJ^5AI�AFĜAD�\AB�yAA�hA@1A?+A>-A=;dA<��A<~�A<I�A<A;
=A9&�A8(�A6�uA5�#A5XA3l�A2n�A1l�A0��A0r�A/l�A.�`A.��A-�#A+��A*�/A(�A(VA'��A&�A&��A%K�A$�A#l�A#�A"��A"��A"�+A �A�Ar�A�A�/Ax�A�AM�A��A�`A �A�A�A��A��A(�A;dA�A��AbA��A��A�PA&�Av�A�PA�A
�A	�A5?A��A��A��A;dAA�A�AffA�-A7LA ��A $�@��R@�9X@��h@���@�@�hs@���@�@��y@���@���@�Z@��;@�33@�E�@�(�@�I�@�+@�@�7@�&�@�=q@���@�@�&�@ۥ�@ڇ+@٩�@��@��
@׮@�S�@�~�@�1'@��`@���@�o@���@̋D@�b@�dZ@�@ʸR@�~�@�5?@ɩ�@�V@Ƨ�@Õ�@���@�1@��@��y@�E�@��@���@���@�G�@��@���@�Q�@��
@�1'@�n�@��@��@��@���@�\)@��@�~�@�M�@�$�@�{@�%@��;@���@�S�@��y@��\@���@�7L@���@��@�Z@���@�l�@�5?@���@��@��H@��@��j@���@�1@���@�+@�n�@��@�I�@��P@�@���@��@�`B@��@���@��@�"�@���@���@�ff@�hs@��P@�;d@�;d@�+@�$�@���@��@���@��@���@�r�@���@�l�@���@���@��+@��#@�X@��@�1@��@�+@�
=@�
=@���@���@��\@��h@�?}@��@���@��`@�bN@�1@�  @��m@��w@�S�@�o@��@�ff@��@��#@��#@�x�@�X@���@���@�Z@� �@�b@���@��m@��F@�l�@��y@���@�V@���@���@�x�@�`B@�7L@��@��`@���@�Ĝ@��j@��j@��u@�z�@�r�@�Z@�I�@�I�@�A�@�1@�ƨ@��F@���@���@�|�@�dZ@�C�@�+@�;d@�+@�+@�K�@��w@���@�ƨ@���@�t�@�\)@�K�@�33@�@��\@�{@��^@��@��@��u@�1'@�;@��@�P@�P@�P@��@|�@�@~V@~@}��@|�@|j@|9X@{�m@{o@zn�@y�@yx�@yx�@yX@y�@x�9@xQ�@w�@wl�@w�@v�+@v$�@u�@u�@uO�@t�@t��@tz�@nff@d��@[��@V��@P��@Jn�@FE�@@1'@:�H@6@2�@,��@'�@ ��@M�@  @��@33@�9@��@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBDBJBJBJBJBJBJBJBJBJBJBJBJBJBJBJBJBJBJBDBDBDBJBDBDBDB
=B
=BDB	7B	7B	7B
=BDBDB
=B	7B1B+B	7B	7B	7B
=B
=B
=B	7B1B%B%BBBBBBBBBBBBBBBBB��B��B��B�B�B��By�B{B�PBs�BW
BH�B?}B.B��B�;B�'B�B�B��B��B�uBw�BS�BM�BH�B>wB/B)�B&�B#�B{B
��B
�B
�B
�NB
��B
��B
�=B
�B
� B
r�B
bNB
W
B
H�B
6FB
�B
B	��B	�B	�B	�sB	�`B	�B	��B	ƨB	�XB	�'B	�B	��B	�VB	�%B	� B	{�B	w�B	k�B	[#B	R�B	M�B	D�B	:^B	/B	$�B	#�B	%�B	%�B	&�B	(�B	(�B	$�B	�B	{B	VB	PB	
=B	%B��B�B�B�mB�TB�HB�;B�)B�B�B�B�B�B��B��BƨBÖB��B�wB�dB�RB�?B�3B�'B�B�B�B��B��B��B��B��B��B��B��B�hB�VB�JB�DB�DB�=B�+B�B~�B|�By�Bt�Bq�Bo�Bl�BjBiyBgmBe`BcTB_;BZBW
BR�BN�BK�BH�BF�BC�B?}B>wB<jB9XB7LB49B2-B0!B/B-B)�B&�B#�B"�B!�B �B�B�B�B�B�B�BoBhBhBbB\BVBVBPBJBJBDB
=B1BB%B%BBBBBBBBBBBBBB  B��B��BBBBBBB%B%B%B%B%BBB%B
=B\BhBhBoBuB{B{B{B{B{B�B{BoB�B�B �B!�B#�B%�B%�B&�B(�B)�B)�B)�B.B2-B33B33B5?B5?B8RB:^B;dB;dB<jB>wB>wBB�BC�BF�BL�BT�BVBVBYBZB\)B]/Be`BgmBk�Bn�Bp�Br�Bv�Bx�By�B{�B� B�B�B�B�B�hB�uB�uB�uB��B��B��B��B��B��B��B��B��B��B��B��B�B�'B�3B�^B�wB��BBBBÖBŢB��B��B��B��B��B�B�#B�#B�)B�/B�HB�TB�`B�yB�B�B�B�B��B��B��B��B	B	B	B	B	B	+B	PB	bB	uB	�B	"�B	%�B	(�B	)�B	.B	.B	.B	/B	/B	0!B	33B	49B	5?B	7LB	:^B	:^B	<jB	>wB	?}B	@�B	B�B	C�B	E�B	F�B	G�B	I�B	J�B	J�B	K�B	M�B	P�B	VB	YB	\)B	]/B	^5B	_;B	`BB	aHB	dZB	dZB	dZB	gmB	o�B	r�B	s�B	s�B	t�B	u�B	v�B	v�B	w�B	{�B	}�B	�B	�B	�B	�1B	�DB	�DB	�JB	�\B	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�?B	��B	�mB	��B
B
hB
�B
#�B
-B
33B
9XB
@�B
F�B
O�B
XB
\)B
_;B
cTB
ffB
l�B
q�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   BB&B$B$B&B&B(B&B&B(B&B&B&B&B&B&B&B$B&BBBB&BBBB
B
BB	B	B	B
BBB
B	BBB	B	B	B
B
B
B	BB BB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B��B��B��B�|B��B��By�BQB�"Bs�BV�BH�B?RB-�B��B�B��B��B��B��B��B�EBw�BS�BM�BH�B>IB.�B)�B&�B#�BLB
��B
�B
�]B
�B
ͣB
��B
�B
��B
�B
r�B
b#B
V�B
H�B
6B
\B
�B	��B	�qB	�ZB	�FB	�7B	��B	ͨB	ƂB	�-B	��B	��B	��B	�/B	��B	�B	{�B	w�B	k]B	Z�B	R�B	M�B	DyB	:9B	.�B	$�B	#�B	%�B	%�B	&�B	(�B	(�B	$�B	�B	UB	1B	+B	
B	�B��B�B�[B�JB�0B�%B�B�B��B��B��B��B��B��BʝBƂB�tB�fB�TB�BB�/B�B�B�B��B��B��B��B��B��B��B��B�yB�rB�]B�EB�3B�'B� B�"B�B�B��B~�B|�By�Bt�Bq�Bo}BliBj]BiZBgMBe>Bc4B_BY�BV�BR�BN�BK�BH�BF�BCxB?\B>UB<HB97B7*B4B2B0 B.�B,�B)�B&�B#�B"�B!�B �B{B�BqBdBsBFB1B,B.B%B BBBBBBB	�B�B�B�BB�B�B�B�B�B�B�B�B �B �B �B �B �B��B��B��B�B�B�B�B�B�B�B�B�BB�B�B�B�B	�B7B*B'BJBPBVBVBVBXB=BABXBIByB�B �B!�B#�B%�B%�B&�B(�B)�B)�B)�B-�B2B3B3
B5B5B8*B:6B;>B;<B<AB>LB>MBBeBCmBF�BL�BT�BU�BU�BX�BY�B[�B]Be3BgDBk[BnmBpyBr�Bv�Bx�By�B{�B�B��B��B��B��B�;B�FB�FB�GB�mB�tB�sB�xB��B��B��B��B��B��B��B��B��B��B�B�2B�HB�YB�aB�aB�aB�iB�qBͣBеBѼB��B��B��B��B��B��B��B�B�#B�-B�KB�dB�kB�lB�B�B��B��B��B	 �B	�B	�B	�B	�B	�B	B	/B	@B	lB	"�B	%�B	(�B	)�B	-�B	-�B	-�B	.�B	.�B	/�B	3B	4B	5B	7B	:(B	:(B	<3B	>AB	?JB	@OB	B[B	C`B	ElB	FtB	GzB	I�B	J�B	J�B	K�B	M�B	P�B	U�B	X�B	[�B	\�B	^B	_B	`B	aB	d"B	d$B	d#B	g8B	okB	ryB	sB	s�B	t�B	u�B	v�B	v�B	w�B	{�B	}�B	��B	��B	��B	��B	�B	�B	�B	�&B	�6B	�IB	�TB	�TB	�UB	�[B	�gB	�mB	�tB	�}B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	ЯB	�3B	�B
�B
,B
]B
#�B
,�B
2�B
9B
@HB
FmB
O�B
W�B
[�B
^�B
cB
f*B
lQB
qp11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.49 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311708222016053117082220160531170822  AO  ARCAADJP                                                                    20150104233119    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150104233119  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20150104233119  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531170822  IP                  G�O�G�O�G�O�                