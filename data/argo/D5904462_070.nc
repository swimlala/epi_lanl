CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-08-17T19:18:07Z AOML 3.0 creation; 2016-08-07T21:51:20Z UW 3.1 conversion     
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
_FillValue                  0  �|   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �(   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �8   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �<   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �L   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �P   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �T   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �XArgo profile    3.1 1.2 19500101000000  20150817191807  20160807145120  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               FA   AO  5287_9017_070                   2C  D   APEX                            6529                            072314                          846 @�hc�\�1   @�hdW?�@0�z�H�d��hr�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    FA   B   B   @9��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BXffB`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B���B���B�  B�33B�33B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(�C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl�Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�fDy��D��D�VfD��3D��fD�fD�FfD��fD��fD��D�C3D�y�D��3D� D�6fDڐ D���D� D�9�D�y�D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @X��@��@ϮA�
A'�
AG�
Ag�
A��A��A��A��A��A��A��A��B��B	��B��B��B!��B)��B1��B9��BA��BI��BQ��BZ\)Ba��Bi��Bq��By��B���B���B���B���B���B���B���B���B�.B���B���B���B���B�ǮB�ǮB���B�.B�.B�ǮB���B���B���B���B���B���B���B���B���B���B���B���B���C }qC}qC}qC}qC}qC
}qC}qC}qC}qC}qC}qC}qC}qC}qC}qC}qC }qC"}qC$}qC&}qC(�C*}qC,}qC.}qC0}qC2}qC4}qC6}qC8}qC:}qC<}qC>}qC@}qCB}qCD}qCF}qCH}qCJ}qCL}qCN}qCP}qCR}qCT}qCV}qCX}qCZ}qC\}qC^}qC`}qCb}qCd}qCf}qCh}qCj}qCl�Cn}qCp}qCr}qCt}qCv}qCx}qCz}qC|}qC~}qC�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�K�C�K�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�D \D �\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D	\D	�\D
\D
�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D \D �\D!\D!�\D"\D"�\D#\D#�\D$\D$�\D%\D%�\D&\D&�\D'\D'�\D(\D(�\D)\D)�\D*\D*�\D+\D+�\D,\D,�\D-\D-�\D.\D.�\D/\D/�\D0\D0�\D1\D1�\D2\D2�\D3\D3�\D4\D4�\D5\D5�\D6\D6�\D7\D7�\D8\D8�\D9\D9�\D:\D:�\D;\D;�\D<\D<�\D=\D=�\D>\D>�\D?\D?�\D@\D@�\DA\DA�\DB\DB�\DC\DC�\DD\DD�\DE\DE�\DF\DF�\DG\DG�\DH\DH�\DI\DI�\DJ\DJ�\DK\DK�\DL\DL�\DM\DM�\DN\DN�\DO\DO�\DP\DP�\DQ\DQ�\DR\DR�\DS\DS�\DT\DT�\DU\DU�\DV\DV�\DW\DW�\DX\DX�\DY\DY�\DZ\DZ�\D[\D[�\D\\D\�\D]\D]�\D^\D^�\D_\D_�\D`\D`�\Da\Da�\Db\Db�\Dc\Dc�\Dd\Dd�\De\De�\Df\Df�\Dg\Dg�\Dh\Dh�\Di\Di�\Dj\Dj�\Dk\Dk�\Dl\Dl�\Dm\Dm�\Dn\Dn�\Do\Do�\Dp\Dp�\Dq\Dq�\Dr\Dr�\Ds\Ds�\Dt\Dt�\Dt��Dy�)D�,{D�fD���D��D�D�VD��D��D�)HD�R�D��HD���D��D�FDڟ�D��{D��D�IHD�HD��{1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�9A�-A�A�^A�wA�wA���A���A�A�A�ĜA�A�ƨA�ȴA�ȴA�ĜA�ȴA�ȴA���A���A���A���A�ȴA�ȴA�O�A���A�5?A���A�\)Aק�A���A�n�A�&�A��Aէ�A�K�A�E�A�{A�ffA�A�dZA�VA�33A��AρA�ĜA�S�A���A˅A�(�AɓuA��`A��/A�ĜA×�A�33A�+A���A�^5A�VA�(�A�7LA��A�dZA��`A�^5A��7A�dZA���A��DA�JA���A��A��A��A���A�p�A�^5A�C�A��A���A��uA��+A���A�A���A��A�$�A��TA���A��A�bA��!A���A���A�^5A�-A�"�A���A�^5A�z�A��A�C�A�v�A�1'A���A���A��A���A~z�Ay��Au��As�Apz�An�RAm/AjbNAfn�Ad(�Ac��A`z�A^��A]oA[%AW��AT1APZAL�/AJ��AH5?ACƨAA�mA>^5A;��A;�A:bA8ffA6�A5?}A3��A1�FA1�A0$�A.ZA,�/A,�!A,ffA+��A*^5A)��A)&�A)�A)"�A(�A%�A!��A jA�A��A�A�AAt�A�A?}AbA�AVA�;A
=A+A��AoA�yA�-A%A�AQ�A$�A�A/A��AZA(�A^5A�mAt�A?}A��A�
A"�A
�9A
��A
��A
v�A
M�A
1A	��A	��A	�PA	33A	A��A9XA�A�mA`BA��AĜA�\A9XA�FA;dA�9AS�A=qA{A$�A(�A�mA��AS�A+A �A ��A bNA Q�A $�@���@��^@���@�Ĝ@�Z@���@�Q�@��m@�ƨ@�33@�@�Ĝ@�~�@�V@�j@�%@�%@�@�w@���@�F@�C�@��#@홚@���@�F@띲@�dZ@�K�@�dZ@�+@���@��@�h@�p�@�&�@旍@���@���@�P@◍@�=q@���@�G�@�j@���@�|�@��
@�|�@�b@��
@�9X@�ƨ@ާ�@ݡ�@ܬ@���@��y@�~�@��T@ج@��@և+@�E�@ա�@�G�@���@թ�@�x�@�z�@ӝ�@��@�33@җ�@�p�@ёh@��`@�r�@��;@��@�{@�J@�V@��@��@�S�@��@��H@ʏ\@ɲ-@���@ȓu@�j@�(�@�l�@�
=@Ɨ�@�M�@�J@�%@��;@�|�@��@��y@���@��T@���@���@�Z@� �@��m@�|�@�\)@�33@��@�"�@�"�@���@�-@���@�?}@���@��@�Z@��@�@�^5@�=q@�V@�n�@�^5@�5?@��@��^@��7@�X@��@�O�@��@���@�Ĝ@��@�j@�(�@���@���@�33@�o@��@��R@�ff@�$�@�@�x�@�`B@�O�@�?}@�7L@�&�@�bN@���@��P@�\)@��@�ȴ@��+@���@���@��!@���@�{@���@���@�I�@��m@��F@���@�dZ@�+@��!@�@��@��m@�33@��H@���@�n�@���@���@�x�@�p�@�x�@�G�@�X@��@���@���@�t�@�+@�ff@��@�?}@�7L@���@�Ĝ@�1@���@�\)@���@�V@�5?@��@���@���@�X@�/@�V@��`@��j@�bN@�1'@��;@�|�@���@���@�-@��#@��7@��@�x�@�`B@�&�@�%@���@��D@�Z@�bN@�bN@�Z@�Z@�I�@� �@�  @��@��;@�ƨ@���@�;d@�n�@�x�@��@�Ĝ@���@��/@���@��9@�j@�ƨ@��H@�n�@�ff@�7L@�+@��
@��;@��-@w;d@m�T@b�@\I�@UV@Kƨ@Ct�@<Z@65?@.��@(��@#t�@  �@1@�@t�@�R1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111 A�9A�-A�A�^A�wA�wA���A���A�A�A�ĜA�A�ƨA�ȴA�ȴA�ĜA�ȴA�ȴA���A���A���A���A�ȴA�ȴA�O�A���A�5?A���A�\)Aק�A���A�n�A�&�A��Aէ�A�K�A�E�A�{A�ffA�A�dZA�VA�33A��AρA�ĜA�S�A���A˅A�(�AɓuA��`A��/A�ĜA×�A�33A�+A���A�^5A�VA�(�A�7LA��A�dZA��`A�^5A��7A�dZA���A��DA�JA���A��A��A��A���A�p�A�^5A�C�A��A���A��uA��+A���A�A���A��A�$�A��TA���A��A�bA��!A���A���A�^5A�-A�"�A���A�^5A�z�A��A�C�A�v�A�1'A���A���A��A���A~z�Ay��Au��As�Apz�An�RAm/AjbNAfn�Ad(�Ac��A`z�A^��A]oA[%AW��AT1APZAL�/AJ��AH5?ACƨAA�mA>^5A;��A;�A:bA8ffA6�A5?}A3��A1�FA1�A0$�A.ZA,�/A,�!A,ffA+��A*^5A)��A)&�A)�A)"�A(�A%�A!��A jA�A��A�A�AAt�A�A?}AbA�AVA�;A
=A+A��AoA�yA�-A%A�AQ�A$�A�A/A��AZA(�A^5A�mAt�A?}A��A�
A"�A
�9A
��A
��A
v�A
M�A
1A	��A	��A	�PA	33A	A��A9XA�A�mA`BA��AĜA�\A9XA�FA;dA�9AS�A=qA{A$�A(�A�mA��AS�A+A �A ��A bNA Q�A $�@���@��^@���@�Ĝ@�Z@���@�Q�@��m@�ƨ@�33@�@�Ĝ@�~�@�V@�j@�%@�%@�@�w@���@�F@�C�@��#@홚@���@�F@띲@�dZ@�K�@�dZ@�+@���@��@�h@�p�@�&�@旍@���@���@�P@◍@�=q@���@�G�@�j@���@�|�@��
@�|�@�b@��
@�9X@�ƨ@ާ�@ݡ�@ܬ@���@��y@�~�@��T@ج@��@և+@�E�@ա�@�G�@���@թ�@�x�@�z�@ӝ�@��@�33@җ�@�p�@ёh@��`@�r�@��;@��@�{@�J@�V@��@��@�S�@��@��H@ʏ\@ɲ-@���@ȓu@�j@�(�@�l�@�
=@Ɨ�@�M�@�J@�%@��;@�|�@��@��y@���@��T@���@���@�Z@� �@��m@�|�@�\)@�33@��@�"�@�"�@���@�-@���@�?}@���@��@�Z@��@�@�^5@�=q@�V@�n�@�^5@�5?@��@��^@��7@�X@��@�O�@��@���@�Ĝ@��@�j@�(�@���@���@�33@�o@��@��R@�ff@�$�@�@�x�@�`B@�O�@�?}@�7L@�&�@�bN@���@��P@�\)@��@�ȴ@��+@���@���@��!@���@�{@���@���@�I�@��m@��F@���@�dZ@�+@��!@�@��@��m@�33@��H@���@�n�@���@���@�x�@�p�@�x�@�G�@�X@��@���@���@�t�@�+@�ff@��@�?}@�7L@���@�Ĝ@�1@���@�\)@���@�V@�5?@��@���@���@�X@�/@�V@��`@��j@�bN@�1'@��;@�|�@���@���@�-@��#@��7@��@�x�@�`B@�&�@�%@���@��D@�Z@�bN@�bN@�Z@�Z@�I�@� �@�  @��@��;@�ƨ@���@�;d@�n�@�x�@��@�Ĝ@���@��/@���@��9@�j@�ƨ@��H@�n�@�ffG�O�@�+@��
@��;@��-@w;d@m�T@b�@\I�@UV@Kƨ@Ct�@<Z@65?@.��@(��@#t�@  �@1@�@t�@�R1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
Q�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
P�B
T�B
\)B
Q�B
O�B
E�B
"�B
+B
6FB
A�B
F�B
W
B
m�B
t�B
{�B
��B
�'B
��BoB<jB�VB��BDB1'BA�BVBgmBr�B��B�\B|�Bw�Bo�Bs�B�B�oB��B�LB��Br�BgmB^5BaHBm�Bu�B�oB�B��B�B��B�B��B�B��B}�B�B��B�hBv�BW
BbB	7B�BɺB�PB`BBQ�BF�B;dB,BK�BYBXBdZBF�B@�B8RB�BB
��B
�B
�fB
�B
ǮB
��B
�%B
YB
.B
JB	��B	�BB	��B	ƨB	�9B	��B	��B	�hB	�B	t�B	iyB	bNB	S�B	?}B	,B	�B	JB	B�B�`B�B��B��BɺB��B��B��B��B��BȴBĜB��BBÖBÖB��B�TB�B	B	+B	JB	�B	�B��B�B��B�B�B��B	B	B	B��B�B�yB�yB�mB�`B�B��B	B	B	JB	\B	hB	hB	oB	�B	�B	�B	�B	%�B	:^B	@�B	E�B	F�B	E�B	F�B	F�B	E�B	N�B	Q�B	S�B	VB	XB	YB	XB	XB	\)B	^5B	^5B	_;B	aHB	bNB	e`B	jB	k�B	l�B	m�B	s�B	t�B	t�B	q�B	o�B	v�B	|�B	�B	�B	�B	�%B	�+B	�1B	�1B	�7B	�7B	�1B	�B	�B	|�B	y�B	|�B	�%B	�+B	�+B	�DB	�PB	�JB	�7B	�B	� B	~�B	�%B	�hB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�'B	�?B	�?B	�FB	�FB	�?B	�9B	�B	�B	�-B	�!B	�B	�B	�B	�B	�B	��B	��B	�B	�-B	�XB	�dB	B	ĜB	ŢB	ƨB	ƨB	ŢB	ÖB	ÖB	ÖB	��B	��B	��B	ÖB	ǮB	ȴB	��B	��B	��B	�
B	�
B	�
B	�B	�
B	�B	�
B	�B	��B	��B	��B	�
B	�B	�B	�#B	�B	��B	��B	��B	��B	�B	�B	�B	�
B	�B	�
B	�B	�B	�#B	�/B	�/B	�B	�B	�B	�B	�B	�
B	�B	��B	��B	��B	��B	�B	�
B	�
B	�B	�B	�B	�)B	�)B	�)B	�)B	�)B	�)B	�#B	�#B	�#B	�/B	�;B	�NB	�ZB	�ZB	�`B	�`B	�fB	�fB	�fB	�fB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	��B	��B	��B	��B
B
B
B
B
B
  B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
	7B
DB
JB
JB
JB
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
PB
PB
VB
VB
\B
\B
VB
VB
PB
JB
DB
DB
+B
DB
hB
"�B
&�B
,B
/B
8RB
=qB
B�B
J�B
P�B
VB
ZB
_;B
dZB
iyB
l�B
o�B
s�B
v�B
z�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111 B
Q�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
P�B
T�B
\	B
Q�B
O�B
E�B
"�B
*�B
6(B
AkB
F�B
V�B
moB
t�B
{�B
��B
�B
��BJB<DB�.B��BB0�BAeBU�BgHBr�B��B�5B|�Bw�BowBs�B��B�EBʗB�$B�\Br�Bg@B^BaBmeBu�B�CB�B��B�yBͫB��B��B��B��B}�B��B�`B�8Bv�BV�B4B	B�eBɍB�$B`BQ�BF}B;6B+�BK�BX�BW�Bd*BF|B@SB8#BrB�B
��B
�hB
�7B
��B
�~B
��B
��B
X�B
-�B
!B	��B	�B	��B	�B	�B	��B	�WB	�?B	��B	t�B	iOB	b*B	S�B	?XB	+�B	uB	$B	 �B�|B�<B��BϼB̩BɖB��B��B��BδBˣBȐB�yB�^B�iB�rB�oBϹB�.B�dB	�B	B	 B	�B	�B��B�B��B�B�}B��B	�B	�B	�B��B�B�OB�PB�EB�5B�B��B	�B	�B	B	1B	?B	=B	DB	VB	gB	gB	�B	%�B	:0B	@XB	EuB	F{B	EwB	FyB	F|B	EuB	N�B	Q�B	S�B	U�B	W�B	X�B	W�B	W�B	[�B	^B	^B	_B	aB	bB	e/B	jQB	kUB	l[B	maB	s�B	t�B	t�B	qyB	ooB	v�B	|�B	��B	��B	��B	��B	��B	��B	� B	�B	�B	� B	��B	��B	|�B	y�B	|�B	��B	��B	��B	�B	�B	�B	�B	��B	�B	~�B	��B	�5B	�7B	�VB	�lB	�mB	�^B	�TB	�[B	�aB	�{B	��B	��B	��B	��B	�B	�B	�B	�B	�	B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�&B	�1B	�\B	�gB	�nB	�sB	�sB	�mB	�_B	�`B	�aB	�UB	�UB	�TB	�bB	�zB	�~B	΢B	ѸB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ϩB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�"B	�&B	�'B	�-B	�/B	�0B	�0B	�BB	�SB	�ZB	�`B	�aB	�gB	�fB	�cB	�fB	�xB	�xB	�B	�}B	�vB	�xB	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�B	�vB	�xB	�yB	�yB	�|B	��B	��B	��B	��B
 �B
�B
�B
�B
 �B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B

B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
 B
"B
B
B
B
B

B
G�O�B
	B
0B
"�B
&�B
+�B
.�B
8B
=3B
BRB
J�B
P�B
U�B
Y�B
^�B
dB
i=B
lPB
o`B
szB
v�B
z�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.49 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071451202016080714512020160807145120  AO  ARCAADJP                                                                    20150817191807    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150817191807  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20150817191807  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807145120  IP                  G�O�G�O�G�O�                