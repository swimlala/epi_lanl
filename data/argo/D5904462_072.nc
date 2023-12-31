CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-08-28T09:16:33Z AOML 3.0 creation; 2016-08-07T21:51:21Z UW 3.1 conversion     
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
_FillValue                    �XArgo profile    3.1 1.2 19500101000000  20150828091633  20160807145121  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               HA   AO  5287_9017_072                   2C  D   APEX                            6529                            072314                          846 @�k����1   @�kffi�@0���+�d� ě��1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    HA   B   B   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A���B   B  B  B  B   B(  B0ffB7��B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B���B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CU�fCX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  DuFfDy�3D��D�C3D���D���D�fD�I�D�y�D�� D�	�D�6fD�i�D�i�D�	�D�L�D�y�D�ɚD���D�33D�i�D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��G@ϮA�
A'�
AG�
Ag�
A��A��A��A��A��A��A��A��RB��B	��B��B��B!��B)��B2\)B9�]BA��BI��BQ��BY��Ba��Bi��Bq��By��B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B�.B�.B�ǮB���B���B���C }qC}qC}qC}qC}qC
}qC}qC}qC}qC}qC}qC}qC}qC}qC}qC}qC }qC"}qC$}qC&}qC(}qC*}qC,}qC.}qC0}qC2}qC4}qC6}qC8}qC:}qC<}qC>}qC@}qCB}qCD}qCF}qCH}qCJ}qCL}qCN}qCP}qCR}qCT}qCVc�CX}qCZ}qC\}qC^}qC`}qCb}qCd}qCf}qCh}qCj}qCl}qCn}qCp}qCr}qCt}qCv}qCx}qCz}qC|}qC~}qC�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�K�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�D \D �\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D	\D	�\D
\D
�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D \D �\D!\D!�\D"\D"�\D#\D#�\D$\D$�\D%\D%�\D&\D&�\D'\D'�\D(\D(�\D)\D)�\D*\D*�\D+\D+�\D,\D,�\D-\D-�\D.\D.�\D/\D/�\D0\D0�\D1\D1�\D2\D2�\D3\D3�\D4\D4�\D5\D5�\D6\D6�\D7\D7�\D8\D8�\D9\D9�\D:\D:�\D;\D;�\D<\D<�\D=\D=�\D>\D>�\D?\D?�\D@\D@�\DA\DA�\DB\DB�\DC\DC�\DD\DD�\DE\DE�\DF\DF�\DG\DG�\DH\DH�\DI\DI�\DJ\DJ�\DK\DK�\DL\DL�\DM\DM�\DN\DN�\DO\DO�\DP\DP�\DQ\DQ�\DR\DR�\DS\DS�\DT\DT�\DU\DU�\DV\DV�\DW\DW�\DX\DX�\DY\DY�\DZ\DZ�\D[\D[�\D\\D\�\D]\D]�\D^\D^�\D_\D_�\D`\D`�\Da\Da�\Db\Db�\Dc\Dc�\Dd\Dd�\De\De�\Df\Df�\Dg\Dg�\Dh\Dh�\Di\Di�\Dj\Dj�\Dk\Dk�\Dl\Dl�\Dm\Dm�\Dn\Dn�\Do\Do�\Dp\Dp�\Dq\Dq�\Dr\Dr�\Ds\Ds�\Dt\Dt�\Du\Due�DyҏD�)HD�R�D��{D��HD�&D�YHD��HD�߮D�HD�FD�yHD�yHD�HD�\{DډHD��HD�{D�B�D�yHD��H1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�A�|�A�z�A�r�A�XA�S�A���A��
A囦A�l�A�^5A�=qA�/A�$�A��A��A�A�A�  A���A�t�A�
=A�{A�1'A�"�A�5?AҶFAґhA�=qA���Aϥ�A��A��A�ZA�jA�A�ĜA�{Aʥ�A�~�A�  A���AɑhAȝ�A���A���A�ffAąA�~�A��A�&�A�5?A�  A�XA��A��hA�oA�(�A���A�E�A�A�9XA��\A�  A���A���A���A�^5A��A�K�A��jA�p�A���A�ĜA�~�A�5?A���A�p�A��FA�1A��A��
A���A���A�&�A��9A���A�r�A�C�A�bA�"�A�bA�r�A���A�A��A��HA~�RAz��Ax�yAuƨAtr�ArAn�!Am�
Al�+Akx�AjQ�Ai/Ahn�AgAd�Aa�A^��A\A�AZ�AY/AU�AR5?AO%AL�AJ��AH�!AHAE�#ACt�AA�TA@=qA?VA=��A<I�A:�/A9A8(�A7p�A4��A3�;A3O�A3VA2��A/�TA-O�A,��A*��A)l�A(=qA'K�A'+A$��A"��A!�
A �A33A`BA��AZA�A�A�jA�wA&�A�jAr�A�A\)A�\AZA��Ax�A��AhsA�HA��An�A��A�At�AdZA�A�A�Ar�AjAr�AZAC�A
5?A	��A	|�A�AJA33A��A1'A�A/AhsAC�AXAM�A�yAz�@���@��P@�
=@���@���@�J@�7L@��^@��@���@���@�1'@�  @�1@�A�@���@�n�@��@��@�@���@��@�h@�/@�j@�"�@��@��;@���@�-@�M�@�5?@��@�@�V@��@�-@��#@��@�h@�X@�/@�?}@�?}@��/@��m@�ƨ@�@�@�\)@��@�^@���@�z�@�@���@�ƨ@�|�@�"�@�\@�hs@�Z@���@ޟ�@�5?@���@ݺ^@ݡ�@�`B@ܬ@�9X@��m@�@ڇ+@�@ٺ^@�p�@��/@ؓu@ו�@���@�J@�O�@���@���@Դ9@ԣ�@ԓu@ԋD@ԃ@�bN@�1@��@щ7@��/@�S�@�E�@�O�@�A�@˝�@�|�@�K�@�+@ʸR@�~�@�v�@�ff@�V@�J@���@Ǿw@�"�@���@�v�@�E�@�$�@�{@���@ź^@�&�@��m@�|�@�;d@�@���@��#@�p�@��9@�z�@�1'@�  @��@���@�@�hs@�`B@�V@�A�@���@���@�M�@�5?@�hs@�7L@���@�Ĝ@��@�bN@�9X@�b@���@��@�
=@�ȴ@��+@�5?@�{@�hs@���@���@�\)@��@��m@�|�@��H@�=q@��@�p�@�%@��9@�(�@��@��@�S�@���@��@��@�/@���@���@�b@��@���@�l�@�@��@�"�@���@���@���@���@�5?@��@��T@��-@�7L@��@���@�Q�@�  @��
@���@��w@�S�@�
=@��@�ȴ@��\@�E�@�$�@�@��@��T@���@���@�p�@�O�@�/@���@��D@�I�@��;@���@��@�t�@�33@��@�
=@�
=@�
=@��@�-@��#@��-@��@�G�@���@�I�@�\)@�33@���@���@�~�@�v�@�V@�M�@���@���@�`B@�&�@�V@���@�r�@�1'@� �@��;@���@�l�@�"�@�ȴ@�n�@�@���@�%@�j@�1'@��@��;@��w@��@��P@�t�@�K�@�
=@���@�V@���@��h@�7L@��/@��j@��@� �@���@�@��@�ȴ@�V@�|�@�V@�
=@�`B@y��@o;d@d�j@\�@SS�@L��@E�@=��@8�9@1hs@+��@(Q�@!�#@ƨ@�y@��@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111 A�A�|�A�z�A�r�A�XA�S�A���A��
A囦A�l�A�^5A�=qA�/A�$�A��A��A�A�A�  A���A�t�A�
=A�{A�1'A�"�A�5?AҶFAґhA�=qA���Aϥ�A��A��A�ZA�jA�A�ĜA�{Aʥ�A�~�A�  A���AɑhAȝ�A���A���A�ffAąA�~�A��A�&�A�5?A�  A�XA��A��hA�oA�(�A���A�E�A�A�9XA��\A�  A���A���A���A�^5A��A�K�A��jA�p�A���A�ĜA�~�A�5?A���A�p�A��FA�1A��A��
A���A���A�&�A��9A���A�r�A�C�A�bA�"�A�bA�r�A���A�A��A��HA~�RAz��Ax�yAuƨAtr�ArAn�!Am�
Al�+Akx�AjQ�Ai/Ahn�AgAd�Aa�A^��A\A�AZ�AY/AU�AR5?AO%AL�AJ��AH�!AHAE�#ACt�AA�TA@=qA?VA=��A<I�A:�/A9A8(�A7p�A4��A3�;A3O�A3VA2��A/�TA-O�A,��A*��A)l�A(=qA'K�A'+A$��A"��A!�
A �A33A`BA��AZA�A�A�jA�wA&�A�jAr�A�A\)A�\AZA��Ax�A��AhsA�HA��An�A��A�At�AdZA�A�A�Ar�AjAr�AZAC�A
5?A	��A	|�A�AJA33A��A1'A�A/AhsAC�AXAM�A�yAz�@���@��P@�
=@���@���@�J@�7L@��^@��@���@���@�1'@�  @�1@�A�@���@�n�@��@��@�@���@��@�h@�/@�j@�"�@��@��;@���@�-@�M�@�5?@��@�@�V@��@�-@��#@��@�h@�X@�/@�?}@�?}@��/@��m@�ƨ@�@�@�\)@��@�^@���@�z�@�@���@�ƨ@�|�@�"�@�\@�hs@�Z@���@ޟ�@�5?@���@ݺ^@ݡ�@�`B@ܬ@�9X@��m@�@ڇ+@�@ٺ^@�p�@��/@ؓu@ו�@���@�J@�O�@���@���@Դ9@ԣ�@ԓu@ԋD@ԃ@�bN@�1@��@щ7@��/@�S�@�E�@�O�@�A�@˝�@�|�@�K�@�+@ʸR@�~�@�v�@�ff@�V@�J@���@Ǿw@�"�@���@�v�@�E�@�$�@�{@���@ź^@�&�@��m@�|�@�;d@�@���@��#@�p�@��9@�z�@�1'@�  @��@���@�@�hs@�`B@�V@�A�@���@���@�M�@�5?@�hs@�7L@���@�Ĝ@��@�bN@�9X@�b@���@��@�
=@�ȴ@��+@�5?@�{@�hs@���@���@�\)@��@��m@�|�@��H@�=q@��@�p�@�%@��9@�(�@��@��@�S�@���@��@��@�/@���@���@�b@��@���@�l�@�@��@�"�@���@���@���@���@�5?@��@��T@��-@�7L@��@���@�Q�@�  @��
@���@��w@�S�@�
=@��@�ȴ@��\@�E�@�$�@�@��@��T@���@���@�p�@�O�@�/@���@��D@�I�@��;@���@��@�t�@�33@��@�
=@�
=@�
=@��@�-@��#@��-@��@�G�@���@�I�@�\)@�33@���@���@�~�@�v�@�V@�M�@���@���@�`B@�&�@�V@���@�r�@�1'@� �@��;@���@�l�@�"�@�ȴ@�n�@�@���@�%@�j@�1'@��@��;@��w@��@��P@�t�@�K�@�
=@���@�V@���@��h@�7L@��/@��j@��@� �@���@�@��@�ȴG�O�@�|�@�V@�
=@�`B@y��@o;d@d�j@\�@SS�@L��@E�@=��@8�9@1hs@+��@(Q�@!�#@ƨ@�y@��@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
�1B
�1B
�1B
�1B
�1B
�+B
�+B
�1B
�1B
�1B
�1B
�+B
�1B
�1B
�1B
�1B
�1B
�=B
�=B
�1B
�VB
��B
BB.BA�BQ�Bp�B�uBĜB�BB��BPB�B{B{B�B�B�B#�B.B1'B5?BH�B[#BhsBq�B�DB��B��B�'B�qB��B��B�;B�yB�;B�BÖB��B�B�ZB�B��B�BB��B��B�-B��B�{B�Bu�Be`B]/BZBP�BG�BVBI�BhB�B�B��B��B��B�BW
B<jBhB
�yB
�jB
��B
� B
n�B
bNB
aHB
/B
DB
VB
oB
B	��B	�fB	��B	ȴB	��B	�XB	�3B	�B	��B	��B	�VB	z�B	n�B	hsB	`BB	W
B	F�B	49B	$�B	�B	�B	�B	�B	hB	1B	B��B��B�B�B�fB�;B�)B�B��B��B��B��B��BȴBǮBŢBɺB�B�`B�yB�B�yB�fB�`B�B�fB�B�yB�B�B	B	DB	bB	bB	uB	�B	�B	�B	!�B	!�B	!�B	!�B	#�B	(�B	+B	/B	2-B	5?B	5?B	7LB	7LB	9XB	<jB	>wB	A�B	A�B	B�B	B�B	D�B	E�B	E�B	K�B	O�B	P�B	M�B	J�B	E�B	/B	$�B	2-B	5?B	:^B	N�B	[#B	T�B	A�B	<jB	>wB	@�B	B�B	H�B	L�B	XB	_;B	_;B	aHB	aHB	cTB	hsB	k�B	l�B	n�B	q�B	z�B	}�B	|�B	t�B	q�B	q�B	r�B	s�B	r�B	r�B	r�B	s�B	u�B	x�B	{�B	�B	�B	�DB	�{B	��B	��B	��B	��B	��B	��B	�B	�?B	�LB	�jB	ĜB	ŢB	ŢB	ŢB	ŢB	ĜB	ŢB	ŢB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�
B	�B	�B	�B	�B	�#B	�#B	�)B	�)B	�)B	�/B	�/B	�/B	�/B	�/B	�5B	�;B	�BB	�HB	�HB	�NB	�NB	�HB	�HB	�NB	�TB	�TB	�TB	�`B	�`B	�`B	�ZB	�TB	�TB	�ZB	�`B	�`B	�ZB	�NB	�NB	�TB	�`B	�`B	�`B	�`B	�`B	�`B	�fB	�fB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
+B
1B
	7B

=B

=B

=B

=B

=B
DB
DB
DB
DB
JB
DB
DB
	7B
1B
1B

=B

=B

=B
DB
JB
JB
VB
VB
PB
VB
VB
VB
\B
bB
bB
hB
hB
bB
\B
VB
JB
JB
JB
PB
PB
VB
\B
\B
\B
\B
bB
bB
hB
oB
uB
uB
uB
uB
uB
uB
oB
hB
\B
\B
bB
oB
�B
oB
{B
 �B
!�B
'�B
.B
9XB
?}B
D�B
K�B
O�B
T�B
YB
^5B
bNB
ffB
jB
o�B
s�B
v�B
z�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111 B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�6B
��B
�kB �B-�BAbBQ�Bp~B�LB�uB�B��B%B`BQBSBjBkB�B#�B-�B0�B5BH�BZ�BhHBq�B�B��B��B� B�HBͫB��B�B�OB�B��B�nB�[B��B�/B�B��B�BʘB�[B�B��B�NB��Bu�Be4B] BY�BP�BG�BU�BI�B;B�B�qB��B��B��B��BV�B<=B9B
�LB
�<B
�uB
�B
nmB
b#B
aB
.�B
B
,B
CB
�B	��B	�=B	ϵB	ȋB	�YB	�/B	�	B	��B	��B	��B	�-B	z�B	nqB	hMB	`B	V�B	F�B	4B	$�B	�B	hB	zB	mB	FB	B	�B��B��B�B�_B�BB�B�B��B��BδBͯB̪BʞBȏBǇB�~BɔB��B�7B�OB�dB�RB�>B�8B�cB�>B�WB�RB�B�|B	�B	B	9B	8B	JB	iB	qB	�B	!�B	!�B	!�B	!�B	#�B	(�B	*�B	.�B	1�B	5B	5B	7B	7B	9+B	<?B	>IB	A\B	A\B	BaB	BbB	DpB	EuB	EsB	K�B	O�B	P�B	M�B	J�B	EtB	.�B	$�B	1�B	5B	:/B	N�B	Z�B	T�B	AZB	<:B	>FB	@UB	BbB	H�B	L�B	W�B	_B	_
B	aB	aB	c#B	hDB	kSB	lYB	ngB	qwB	z�B	}�B	|�B	t�B	qwB	qvB	r�B	s�B	rB	r}B	r~B	s�B	u�B	x�B	{�B	��B	��B	�B	�HB	�gB	�B	��B	��B	��B	��B	��B	�B	�B	�6B	�fB	�nB	�mB	�oB	�lB	�hB	�mB	�nB	ʎB	̙B	̘B	͜B	ΤB	ЯB	ЮB	ѷB	ѴB	ҽB	ҾB	ҾB	��B	ҿB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ѹB	ѶB	ҾB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�(B	�*B	�(B	�#B	�B	�B	�#B	�*B	�)B	�#B	�B	�B	�B	�*B	�(B	�(B	�*B	�)B	�&B	�0B	�.B	�5B	�:B	�BB	�MB	�SB	�]B	�_B	�`B	�`B	�_B	�_B	�rB	�qB	�fB	�fB	�]B	�fB	�~B	��B	��B	��B	��B	��B	�B	�wB	�qB	�nB	�sB	�rB	�lB	�fB	�eB	�gB	�rB	�B	�}B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B

B

B

B

B

B
B
B
B
B
B

B
B
�B
�B
�B

B

B

B
B
B
B
B
B
B
B
B
B
&B
)B
*B
/B
1B
+B
"B
B
B
B
B
B
B
B
$B
"B
#B
%B
*B
)B
.B
4B
;B
;B
:B
<B
=B
=B
3B
-B
!B
"B
(B
4G�O�B
7B
AB
 �B
!�B
'�B
-�B
9B
?CB
DaB
K�B
O�B
T�B
X�B
]�B
bB
f*B
jDB
oaB
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.49 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071451212016080714512120160807145121  AO  ARCAADJP                                                                    20150828091633    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150828091633  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20150828091633  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807145121  IP                  G�O�G�O�G�O�                