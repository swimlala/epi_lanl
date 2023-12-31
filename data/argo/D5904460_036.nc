CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-03-04T20:17:52Z AOML 3.0 creation; 2016-08-07T21:17:34Z UW 3.1 conversion     
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
_FillValue                 �  Ax   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  Cp   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  KH   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M@   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  U   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ^�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  f�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  p�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  xh   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  z`   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �8   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �0   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �8   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �8   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �8   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �8   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �d   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �h   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �l   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �p   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �t   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20150304201752  20160807141734  5904460 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               $A   AO  5285_8895_036                   2C  D   APEX                            6487                            072314                          846 @�>���?�1   @�>�-��
@,<�hr��c�dZ�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    $A   B   B   @�ff@�  A��A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bi33Bo33Bw��B�  B�  B�  B�  B�  B�33B�  B���B���B���B�  B�  B�  B�  B�33B�  B���B�  B�  B�  B�  B�  B�33B�33B���B�  B�  B�  B�  B�  B�  B�  C   C  C  C�C  C
  C  C  C  C�fC  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2�C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CK�fCN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZfDZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt� Dy�fD�fD�C3D��3D�� D�  D�P D�|�D��3D�3D�I�D�y�Dǹ�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�z@ϮA	p�A'�
AG�
Ag�
A��A��A��A��A��A��A��A��B��B	��B��B��B!��B)��B1��B9��BA��BI��BQ��BY��Ba��Bk(�Bq(�By�]B���B���B���B���B���B�.B���B�ǮB�ǮB�ǮB���B���B���B���B�.B���B�ǮB���B���B���B���B���B�.B�.B�ǮB���B���B���B���B���B���B���C }qC}qC}qC�C}qC
}qC}qC}qC}qCc�C}qC}qC}qC}qC}qC}qC }qC"}qC$}qC&}qC(}qC*}qC,}qC.}qC0}qC2�C4}qC6}qC8}qC:}qC<}qC>}qC@}qCB}qCD}qCF}qCH}qCJ}qCLc�CN}qCP}qCR}qCT}qCV}qCX}qCZ}qC\}qC^}qC`}qCb}qCd}qCf}qCh}qCj}qCl}qCn}qCp}qCr}qCt}qCv}qCx}qCz}qC|}qC~}qC�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�D \D �\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D	\D	�\D
\D
�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D%�D�\D\D�\D\D�\D\D�\D\D�\D \D �\D!\D!�\D"\D"�\D#\D#�\D$\D$�\D%\D%�\D&\D&�\D'\D'�\D(\D(�\D)\D)�\D*\D*�\D+\D+�\D,\D,�\D-\D-�\D.\D.�\D/\D/�\D0\D0�\D1\D1�\D2\D2�\D3\D3�\D4\D4�\D5\D5�\D6\D6�\D7\D7�\D8\D8�\D9\D9�\D:\D:�\D;\D;�\D<\D<�\D=\D=�\D>\D>�\D?\D?�\D@\D@�\DA\DA�\DB\DB�\DC\DC�\DD\DD�\DE\DE�\DF\DF�\DG\DG�\DH\DH�\DI\DI�\DJ\DJ�\DK\DK�\DL\DL�\DM\DM�\DN\DN�\DO\DO�\DP\DP�\DQ\DQ�\DR\DR�\DS\DS�\DT\DT�\DU\DU�\DV\DV�\DW\DW�\DX\DX�\DY\DY�\DZ%�DZ�\D[\D[�\D\\D\�\D]\D]�\D^\D^�\D_\D_�\D`\D`�\Da\Da�\Db\Db�\Dc\Dc�\Dd\Dd�\De\De�\Df\Df�\Dg\Dg�\Dh\Dh�\Di\Di�\Dj\Dj�\Dk\Dk�\Dl\Dl�\Dm\Dm�\Dn\Dn�\Do\Do�\Dp\Dp�\Dq\Dq�\Dr\Dr�\Ds\Ds�\Dt\Dt�\Dt�\Dy��D�D�R�D���D�߮D�/�D�_�D��{D���D��D�YHD��HD��H1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�|�AօAօAօA։7A֑hA֑hA֓uA֓uA֓uA֑hA֑hA֓uA֕�A֗�A֗�A֙�A֙�A֛�A֛�A֛�A֗�A֕�A֑hA�z�A�`BA��TA�5?A�x�A�A�A���AЙ�Aа!A��A�^5A���Aϟ�A�K�A�  A�v�A�%A�z�A��TAȴ9AǼjA��AőhA��A�A��A��PA��yA�
=A�%A��A�A�+A�=qA�5?A�&�A�~�A�O�A���A�ĜA��A��A�^5A��A�bA���A��A�|�A�^5A�;dA��A���A��A�1'A��TA~VA{%Ay`BAvv�Apv�Am33AjE�Aex�Ab�A^Q�AZE�ATbAO�AN1AJA�AG7LAD�ABĜAB�AAO�A@A>�DA;�A9?}A7�7A65?A5�A5&�A3�A1��A0��A0��A0��A0�DA.��A-K�A,-A+&�A)l�A'`BA%�FA#�mA!?}A�;A&�A$�A|�A�yAI�A�wA7LA$�A�hAx�A33A�A$�A�DA��AA�`A9XA�A�
AG�A?}A��A=qA��A��A�A/A
{A~�A1A�TA?}AM�A�;A��A?}A�!Az�A�wA/A �jA V@��
@���@��wA 1@�t�@�-@��-@��^@�G�@���@��u@��@�ȴ@���@�/@��@��@���@���@��!@�33@�hs@���@��7@�-@��R@�~�@�E�@��@�%@�@��;@�!@���@��D@�Q�@�b@�w@�t�@�33@�ȴ@�@�ff@�{@�$�@�\@웦@�hs@�V@�@�l�@���@�=q@�/@�w@�33@�+@�{@��@�I�@� �@�  @�"�@��H@�v�@��@��T@�@�ƨ@އ+@�ff@���@�O�@�z�@�  @���@���@ە�@��#@ٙ�@�x�@���@أ�@��/@ؼj@� �@ו�@��H@�{@պ^@��/@�bN@��
@���@Ӿw@�@���@���@�ȴ@�J@�V@��/@�z�@�9X@ϕ�@��@ͺ^@͉7@�&�@̼j@̋D@�9X@˅@�+@��@�v�@�{@ɩ�@�&�@���@�1'@ǶF@ǅ@��@���@�ff@��@�&�@Ĭ@�9X@���@Ý�@�t�@���@�v�@�E�@�$�@��@���@���@�O�@�I�@��@���@��@�\)@�"�@���@��\@�ff@�M�@��@���@�@�x�@�7L@���@��u@�1'@��m@�t�@���@���@��T@��^@��7@�`B@�%@���@�r�@��@�dZ@�+@�o@��H@��+@�M�@���@��@�&�@���@���@�b@��
@��@���@�K�@���@���@�^5@���@�p�@�&�@��D@���@���@�K�@��y@��!@�E�@��@��h@�&�@�V@��9@��@��@�|�@�K�@�@���@�=q@���@�`B@��@��`@��j@�Q�@��@���@�\)@�"�@���@���@�E�@��@��^@��h@�`B@�G�@�7L@��@���@��@�I�@��P@�dZ@�C�@���@��@���@�M�@���@��7@��@���@�Ĝ@��D@�1'@��@���@��@�C�@��@��H@��!@�^5@�$�@���@���@���@���@�x�@�?}@��@�%@��j@��D@���@���@�l�@�S�@�+@�^5@��@�{@���@�G�@��@�V@��@�  @��@���@�|�@�t�@�;d@��@��!@�n�@�E�@�{@���@�X@�?}@���@���@��@��
@�l�@��@���@��@�v�@�J@�@��@�I�@� �@�1@��P@�;d@���@��y@��H@��@���@��+@�-@���@��^@��7@�%@���@��@��u@v$�@lz�@c��@Y�@Rn�@J��@B�@;"�@5�@.�R1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111  A�|�AօAօAօA։7A֑hA֑hA֓uA֓uA֓uA֑hA֑hA֓uA֕�A֗�A֗�A֙�A֙�A֛�A֛�A֛�A֗�A֕�A֑hA�z�A�`BA��TA�5?A�x�A�A�A���AЙ�Aа!A��A�^5A���Aϟ�A�K�A�  A�v�A�%A�z�A��TAȴ9AǼjA��AőhA��A�A��A��PA��yA�
=A�%A��A�A�+A�=qA�5?A�&�A�~�A�O�A���A�ĜA��A��A�^5A��A�bA���A��A�|�A�^5A�;dA��A���A��A�1'A��TA~VA{%Ay`BAvv�Apv�Am33AjE�Aex�Ab�A^Q�AZE�ATbAO�AN1AJA�AG7LAD�ABĜAB�AAO�A@A>�DA;�A9?}A7�7A65?A5�A5&�A3�A1��A0��A0��A0��A0�DA.��A-K�A,-A+&�A)l�A'`BA%�FA#�mA!?}A�;A&�A$�A|�A�yAI�A�wA7LA$�A�hAx�A33A�A$�A�DA��AA�`A9XA�A�
AG�A?}A��A=qA��A��A�A/A
{A~�A1A�TA?}AM�A�;A��A?}A�!Az�A�wA/A �jA V@��
@���@��wA 1@�t�@�-@��-@��^@�G�@���@��u@��@�ȴ@���@�/@��@��@���@���@��!@�33@�hs@���@��7@�-@��R@�~�@�E�@��@�%@�@��;@�!@���@��D@�Q�@�b@�w@�t�@�33@�ȴ@�@�ff@�{@�$�@�\@웦@�hs@�V@�@�l�@���@�=q@�/@�w@�33@�+@�{@��@�I�@� �@�  @�"�@��H@�v�@��@��T@�@�ƨ@އ+@�ff@���@�O�@�z�@�  @���@���@ە�@��#@ٙ�@�x�@���@أ�@��/@ؼj@� �@ו�@��H@�{@պ^@��/@�bN@��
@���@Ӿw@�@���@���@�ȴ@�J@�V@��/@�z�@�9X@ϕ�@��@ͺ^@͉7@�&�@̼j@̋D@�9X@˅@�+@��@�v�@�{@ɩ�@�&�@���@�1'@ǶF@ǅ@��@���@�ff@��@�&�@Ĭ@�9X@���@Ý�@�t�@���@�v�@�E�@�$�@��@���@���@�O�@�I�@��@���@��@�\)@�"�@���@��\@�ff@�M�@��@���@�@�x�@�7L@���@��u@�1'@��m@�t�@���@���@��T@��^@��7@�`B@�%@���@�r�@��@�dZ@�+@�o@��H@��+@�M�@���@��@�&�@���@���@�b@��
@��@���@�K�@���@���@�^5@���@�p�@�&�@��D@���@���@�K�@��y@��!@�E�@��@��h@�&�@�V@��9@��@��@�|�@�K�@�@���@�=q@���@�`B@��@��`@��j@�Q�@��@���@�\)@�"�@���@���@�E�@��@��^@��h@�`B@�G�@�7L@��@���@��@�I�@��P@�dZ@�C�@���@��@���@�M�@���@��7@��@���@�Ĝ@��D@�1'@��@���@��@�C�@��@��H@��!@�^5@�$�@���@���@���@���@�x�@�?}@��@�%@��j@��D@���@���@�l�@�S�@�+@�^5@��@�{@���@�G�@��@�V@��@�  @��@���@�|�@�t�@�;d@��@��!@�n�@�E�@�{@���@�X@�?}@���@���@��@��
@�l�@��@���@��@�v�@�J@�@��@�I�@� �@�1@��P@�;d@���@��y@��H@��@���@��+@�-@���@��^@��7G�O�@���@��@��u@v$�@lz�@c��@Y�@Rn�@J��@B�@;"�@5�@.�R1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;oBu�Bu�Bu�Bu�Bu�Bu�Bu�Bu�Bu�Bu�Bv�Bv�Bv�Bv�Bw�Bw�Bv�Bv�Bv�Bw�Bw�By�B{�B|�B�B�JB�'B�;B	bB	cTB	�-B
49B
�dB
�B�B�B�B,B�B\BA�BW
Bp�B��B1'BbNBu�B�1B\B��B��B��B��B�RB�B�B��B"�B�B5?B�BJ�B;dB-B�B�
B��B�PB�wB�3B�7BM�B
�B
ŢB
��B
m�B
Q�B
8RB
oB	�/B	�dB	�B	�3B	�oB	~�B	q�B	`BB	R�B	B�B	/B	PB��B��B�B�B�B�B�yB�B�B�mB�B�B�B�B�B�B	%B	bB	�B	�B	�B	�B	&�B	0!B	7LB	9XB	8RB	7LB	6FB	49B	49B	2-B	.B	/B	5?B	<jB	<jB	@�B	B�B	G�B	L�B	M�B	M�B	N�B	N�B	O�B	N�B	M�B	K�B	K�B	K�B	J�B	J�B	O�B	Q�B	R�B	ZB	]/B	e`B	jB	jB	l�B	k�B	iyB	ffB	cTB	aHB	]/B	T�B	Q�B	R�B	VB	W
B	YB	ZB	ZB	\)B	dZB	jB	l�B	s�B	s�B	w�B	x�B	z�B	}�B	}�B	|�B	{�B	~�B	�B	�+B	�7B	�+B	�bB	��B	��B	��B	��B	�B	�9B	�XB	�XB	�XB	�^B	�wB	�}B	ÖB	ÖB	ɺB	��B	ɺB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	�B	��B	�5B	�5B	�;B	�BB	�BB	�5B	�/B	�B	�
B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	�
B	�B	�B	�
B	�B	�B	�HB	�;B	�B	�B	�/B	�BB	�HB	�TB	�`B	�ZB	�TB	�NB	�HB	�BB	�5B	�BB	�;B	�HB	�TB	�TB	�TB	�mB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
%B
%B
+B
+B
1B
1B
1B
1B
	7B
	7B
	7B

=B

=B

=B

=B

=B
DB
DB
JB
DB
JB
JB
JB
JB
JB
DB
JB
PB
PB
PB
VB
VB
VB
VB
VB
VB
VB
VB
VB
\B
\B
\B
bB
bB
hB
oB
oB
oB
oB
oB
uB
uB
uB
uB
uB
uB
uB
uB
uB
uB
{B
{B
{B
{B
uB
uB
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
�B
 �B
 �B
�B
 �B
 �B
"�B
#�B
#�B
#�B
#�B
#�B
#�B
0!B
%�B
,B
1'B
8RB
?}B
H�B
M�B
R�B
W
B
]/B
`BB
dZB
hs1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111  Bu�Bu�Bu�Bu�Bu�Bu�Bu�Bu�Bu�Bu�Bv�Bv�Bv�Bv�Bw�Bw�Bv�Bv�Bv�Bw�Bw�By�B{�B|�B��B�'B�B�+B	MB	c:B	�B
4B
�BB
�^BuB�B�B+�B_B5BAeBV�Bp|B�[B0�Bb#Bu�B�	B1B��B��B��B�|B�)B��B��B��B"�B�B5B{BJ�B;:B,�BYB��B��B�"B�HB�B�BM�B
�B
�uB
�tB
mbB
Q�B
8(B
CB	�B	�=B	��B	�B	�HB	~�B	q�B	`B	R�B	BlB	.�B	+B��B��B�tB�ZB�aB�[B�TB�aB�[B�HB�dB�zB�qB�B�B�B	�B	9B	dB	�B	�B	�B	&�B	/�B	7!B	9+B	8'B	7B	6B	4B	4B	2B	-�B	.�B	5B	<=B	<?B	@XB	BdB	G�B	L�B	M�B	M�B	N�B	N�B	O�B	N�B	M�B	K�B	K�B	K�B	J�B	J�B	O�B	Q�B	R�B	Y�B	\�B	e2B	jNB	jMB	l[B	kVB	iIB	f6B	c%B	aB	\�B	T�B	Q�B	R�B	U�B	V�B	X�B	Y�B	Y�B	[�B	d*B	jOB	lXB	s�B	s�B	w�B	x�B	z�B	}�B	}�B	|�B	{�B	~�B	��B	��B	�B	��B	�2B	�YB	�OB	�UB	�}B	��B	�B	�$B	�#B	�$B	�+B	�FB	�IB	�bB	�aB	ɅB	ʎB	ɆB	ɅB	ɅB	˔B	͛B	бB	ѶB	ҽB	��B	��B	��B	�B	�B	�B	�B	�B	� B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ҾB	ѷB	бB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	�B	�B	�B	�+B	�%B	�B	�B	�B	�
B	�B	�B	�B	�B	� B	�B	�B	�6B	�VB	�OB	�GB	�SB	�aB	�ZB	�UB	�FB	�GB	�BB	�GB	�GB	�UB	�\B	�ZB	�`B	�eB	�gB	�fB	�gB	�nB	�lB	�mB	�xB	�zB	�~B	�~B	�xB	�uB	�sB	�mB	�lB	�lB	�qB	�kB	�lB	�qB	�sB	�yB	�tB	�B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B

B

B

B

B

B

B
B
B
	B
B
B
B
B
B

B
B
B
B
B
B
B
B
B
B
B
B
B
B
#B
$B
#B
)B
,B
,B
5B
6B
3B
8B
5B
=B
=B
=B
;B
=B
<B
:B
<B
<B
:B
?B
AB
@B
CB
;B
:B
AB
AB
AB
DB
FB
EB
SB
UB
UB
`B
eB
cB
fB
kB
mB
lB
dB
eB
mB
mB
dB
dB
dB
qB
rB
oB
pB
rB
pB
zB
wB
zB
yB
rB
xB
{B
 �B
 �B
�B
 �B
 �B
"�B
#�B
#�B
#�B
#�B
#�B
#�G�O�B
%�B
+�B
0�B
8B
?BB
HyB
M�B
R�B
V�B
\�B
`B
dB
h91111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.49 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071417342016080714173420160807141734  AO  ARCAADJP                                                                    20150304201752    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150304201752  QCP$                G�O�G�O�G�O�DFB5E           AO  ARGQQCPL                                                                    20150304201752  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807141734  IP                  G�O�G�O�G�O�                