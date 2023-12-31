CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2016-04-16T09:16:25Z AOML 3.0 creation; 2016-08-07T21:51:28Z UW 3.1 conversion     
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
_FillValue                    gx   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ix   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  qp   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yh   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {h   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �`   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �`   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �X   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �(   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �,   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �0   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20160416091625  20160807145128  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               tA   AO  5287_9017_116                   2C  D   APEX                            6529                            072314                          846 @ץ����1   @ץ��E�@0%�Q��d�`A�7L1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    tA   B   B   @�33@�  A   A   A@  A`  A�  A�  A�  A���A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�ffB�ffB���B�  B�  B�  B�33B�33B���B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C�fC
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C'�fC*  C,  C.  C0�C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D'��D(� D)  D)y�D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3fD3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�3Dy� D�fD�S3D�� D��3D��D�@ D�p D�� D��D�33D�c3D��3D�fD�` Dڃ3D�ɚD�	�D�S3D�vfD�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��G@ϮA�
A'�
AG�
Ag�
A��A��A��A��RA��A��A��A��B��B	��B��B��B!��B)��B1��B9��BA��BI��BQ��BY��Ba��Bi��Bq��By��B���B���B���B���B�.B���B���B���B���B���B���B���B���B���B���B�aGB�aGB�ǮB���B���B���B�.B�.B�ǮB���B���B���B���B���B���B���B���C }qC}qC}qC}qCc�C
}qC}qC}qC}qC}qC}qC}qC}qC}qC}qC}qC }qC"}qC$}qC&}qC(c�C*}qC,}qC.}qC0�C2}qC4}qC6}qC8}qC:}qC<}qC>}qC@}qCB}qCD}qCF}qCH}qCJ}qCL}qCN}qCP}qCR}qCT}qCV}qCX}qCZ}qC\}qC^}qC`}qCb}qCd}qCf}qCh}qCj}qCl}qCn}qCp}qCr}qCt}qCv}qCx}qCz}qC|}qC~}qC�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�D \D �\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D	\D	�\D
\D
�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D \D �\D!\D!�\D"\D"�\D#\D#�\D$\D$�\D%\D%�\D&\D&�\D'\D'�\D(�D(�\D)\D)��D*\D*�\D+\D+�\D,\D,�\D-\D-�\D.\D.�\D/\D/�\D0\D0�\D1\D1�\D2\D2�\D3%�D3�\D4\D4�\D5\D5�\D6\D6�\D7\D7�\D8\D8�\D9\D9�\D:\D:�\D;\D;�\D<\D<�\D=\D=�\D>\D>�\D?\D?�\D@\D@�\DA\DA�\DB\DB�\DC\DC�\DD\DD�\DE\DE�\DF\DF�\DG\DG�\DH\DH�\DI\DI�\DJ\DJ�\DK\DK�\DL\DL�\DM\DM�\DN\DN�\DO\DO�\DP\DP�\DQ\DQ�\DR\DR�\DS\DS�\DT\DT�\DU\DU�\DV\DV�\DW\DW�\DX\DX�\DY\DY�\DZ\DZ�\D[\D[�\D\\D\�\D]\D]�\D^\D^�\D_\D_�\D`\D`�\Da\Da�\Db\Db�\Dc\Dc�\Dd\Dd�\De\De�\Df\Df�\Dg\Dg�\Dh\Dh�\Di\Di�\Dj\Dj�\Dk\Dk�\Dl\Dl�\Dm\Dm�\Dn\Dn�\Do\Do�\Dp\Dp�\Dq\Dq�\Dr\Dr�\Ds\Ds�\Dt\Dt�\Dt�Dy�\D�&D�b�D���D���D�{D�O�D��D�ϮD�{D�B�D�r�D���D�D�o�Dڒ�D��HD�HD�b�D�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A�  A��mA��yA��A���A���A��/A��A��A��TA��HA��mAρA�^5A�^5A�\)A�ZA�XA�VA�S�A�VA�Q�A�O�A�M�A�K�A�I�A�G�A�A�A�9XA�(�AξwA��Aͥ�A�  A̟�A��A˅A��HA�-A��HA�/Aȉ7A�&�A��A��A��A�A�A�A���A��A�9XA��A�1'A��FA�;dA��A��A���A��jA��7A���A��A��^A�p�A�r�A�5?A��A�A���A�
=A�O�A�1A��DA���A��A�5?A�JA���A�Q�A���A���A�5?A�;dA���A�ĜA�/A�\)A�;dA���A�~�A���A�ffA���A�`BA��A��TA~�Aw�7AtE�Aq�^Ao+Al1'AjM�AgoAb��A^z�A\��A\ZAZ~�ASl�AR�RARQ�AQ�FAQ��AQ��AP�AM�^AJ�jAE
=AB��AAVA@Q�A?
=A<ffA8A4��A2�+A/��A.^5A-t�A+�-A*{A)x�A(��A&��A$  A"�uA!t�A�AdZA��A��A�A��AQ�A"�A �A�A�PA�hA
=AM�A��A�/A"�A�RA�A�A�uAƨA|�A"�A�A��A�A/A
�uA	A	&�A��A��A�A�A;dA"�AVA��A�DAI�AA�AM�A�AhsA j@��F@�dZ@��!@��h@��9@� �@��m@�"�@��!@�v�@���@��7@�7L@�  @�\)@���@�7L@�+@�?}@�j@�l�@�{@�x�@�Q�@�|�@�+@�{@�/@���@�|�@�$�@�x�@��@�7L@�@ߥ�@�33@��@ާ�@އ+@�n�@�V@�5?@���@�p�@�&�@�V@���@�1'@�  @�ƨ@��y@�=q@��@�@�/@��`@�Ĝ@�I�@�t�@�@֧�@�v�@�E�@�-@��@�?}@�%@���@�  @ӥ�@�S�@��@��y@җ�@��T@��@�z�@�  @��m@Ͼw@�C�@���@Ο�@���@͑h@�X@���@��
@�dZ@�J@ɉ7@�Z@�33@Ɨ�@�5?@��@Ł@���@�  @�l�@�C�@��y@�V@�%@�Q�@�;d@��R@���@�v�@�=q@�G�@�(�@�K�@��@��+@��@���@�X@���@�1@�"�@���@��@�V@�J@��@��#@�@�x�@�X@�&�@���@�j@� �@�1@��@��m@��;@��
@�ƨ@��y@��@���@��7@�O�@�O�@�G�@�O�@�%@���@�r�@���@��
@���@�t�@�;d@�5?@��`@�I�@�(�@� �@� �@� �@���@��F@�l�@�
=@�ȴ@���@�M�@�p�@�Z@��m@�\)@�"�@��@��R@�$�@��^@�p�@�O�@�/@���@�Z@���@�S�@�S�@�;d@��@��R@��+@�E�@���@�hs@�X@�?}@�/@��j@�(�@�  @���@�;d@�@��y@���@�n�@���@�p�@�O�@��@���@��9@�Q�@�I�@�Q�@� �@��@�1'@�(�@��
@��F@��F@���@���@���@���@��P@���@��h@��9@��@�r�@�bN@�9X@�b@��;@��@��@�;d@��@���@�v�@�E�@��^@�X@�&�@��@���@�z�@�r�@�A�@�ƨ@�t�@�@���@���@��+@�E�@�J@��^@�G�@�%@���@�Ĝ@�z�@�j@�(�@�1@�K�@��!@���@�%@�Ĝ@��j@���@�z�@�1'@���@��@��;@���@�t�@�S�@�K�@�+@�@��@���@�v�@�-@��@��H@��@�@��-@��h@�hs@�O�@�?}@���@���@���@�Z@��@yG�@kt�@`r�@Y��@T�D@L�@G�;@A��@6v�@2^5@*��@%��@"�@�-@�y@dZ@b@
�\@
�\111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A���A�  A��mA��yA��A���A���A��/A��A��A��TA��HA��mAρA�^5A�^5A�\)A�ZA�XA�VA�S�A�VA�Q�A�O�A�M�A�K�A�I�A�G�A�A�A�9XA�(�AξwA��Aͥ�A�  A̟�A��A˅A��HA�-A��HA�/Aȉ7A�&�A��A��A��A�A�A�A���A��A�9XA��A�1'A��FA�;dA��A��A���A��jA��7A���A��A��^A�p�A�r�A�5?A��A�A���A�
=A�O�A�1A��DA���A��A�5?A�JA���A�Q�A���A���A�5?A�;dA���A�ĜA�/A�\)A�;dA���A�~�A���A�ffA���A�`BA��A��TA~�Aw�7AtE�Aq�^Ao+Al1'AjM�AgoAb��A^z�A\��A\ZAZ~�ASl�AR�RARQ�AQ�FAQ��AQ��AP�AM�^AJ�jAE
=AB��AAVA@Q�A?
=A<ffA8A4��A2�+A/��A.^5A-t�A+�-A*{A)x�A(��A&��A$  A"�uA!t�A�AdZA��A��A�A��AQ�A"�A �A�A�PA�hA
=AM�A��A�/A"�A�RA�A�A�uAƨA|�A"�A�A��A�A/A
�uA	A	&�A��A��A�A�A;dA"�AVA��A�DAI�AA�AM�A�AhsA j@��F@�dZ@��!@��h@��9@� �@��m@�"�@��!@�v�@���@��7@�7L@�  @�\)@���@�7L@�+@�?}@�j@�l�@�{@�x�@�Q�@�|�@�+@�{@�/@���@�|�@�$�@�x�@��@�7L@�@ߥ�@�33@��@ާ�@އ+@�n�@�V@�5?@���@�p�@�&�@�V@���@�1'@�  @�ƨ@��y@�=q@��@�@�/@��`@�Ĝ@�I�@�t�@�@֧�@�v�@�E�@�-@��@�?}@�%@���@�  @ӥ�@�S�@��@��y@җ�@��T@��@�z�@�  @��m@Ͼw@�C�@���@Ο�@���@͑h@�X@���@��
@�dZ@�J@ɉ7@�Z@�33@Ɨ�@�5?@��@Ł@���@�  @�l�@�C�@��y@�V@�%@�Q�@�;d@��R@���@�v�@�=q@�G�@�(�@�K�@��@��+@��@���@�X@���@�1@�"�@���@��@�V@�J@��@��#@�@�x�@�X@�&�@���@�j@� �@�1@��@��m@��;@��
@�ƨ@��y@��@���@��7@�O�@�O�@�G�@�O�@�%@���@�r�@���@��
@���@�t�@�;d@�5?@��`@�I�@�(�@� �@� �@� �@���@��F@�l�@�
=@�ȴ@���@�M�@�p�@�Z@��m@�\)@�"�@��@��R@�$�@��^@�p�@�O�@�/@���@�Z@���@�S�@�S�@�;d@��@��R@��+@�E�@���@�hs@�X@�?}@�/@��j@�(�@�  @���@�;d@�@��y@���@�n�@���@�p�@�O�@��@���@��9@�Q�@�I�@�Q�@� �@��@�1'@�(�@��
@��F@��F@���@���@���@���@��P@���@��h@��9@��@�r�@�bN@�9X@�b@��;@��@��@�;d@��@���@�v�@�E�@��^@�X@�&�@��@���@�z�@�r�@�A�@�ƨ@�t�@�@���@���@��+@�E�@�J@��^@�G�@�%@���@�Ĝ@�z�@�j@�(�@�1@�K�@��!@���@�%@�Ĝ@��j@���@�z�@�1'@���@��@��;@���@�t�@�S�@�K�@�+@�@��@���@�v�@�-@��@��H@��@�@��-@��h@�hs@�O�@�?}@���@���G�O�@�Z@��@yG�@kt�@`r�@Y��@T�D@L�@G�;@A��@6v�@2^5@*��@%��@"�@�-@�y@dZ@b@
�\@
�\111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
ƨB
ƨB
ƨB
ƨB
ƨB
ƨB
ƨB
ƨB
ƨB
ƨB
ƨB
ƨB
ƨB
ĜB
ÖB
ÖB
ÖB
ÖB
ÖB
ÖB
ÖB
ÖB
B
B
B
B
B
B
B
ÖB
ŢB
��B
�B
�HB
�B
�B
��B1B,B=qB@�BT�B�B��B��B�)B��B\B��B�Bk�BdZB]/Bt�B��BdZBw�BS�B8RB(�B�B{BVB1B��B�B�HB��BǮB��B�^B�!B��B��B��B�bB�Bu�BcTBL�B7LB,B(�B"�B�BG�B?}B:^B/B!�B�BuB	7B
��B
��B
��B
�B
q�B
T�B
!�B
B	�B	�)B	ŢB	�LB	��B	�=B	t�B	l�B	ffB	XB	49B	0!B	-B	)�B	)�B	'�B	!�B	uB	+B��B�B�B�fB�;B��BȴB��B�}B��B��B��BBBB��B��BƨBB��B�}B�wB�qB�dB�RB�LB�FB�9B�-B�LB�dBB��BɺB��BȴBȴBȴBĜBÖB��B�}B�}B�qB�qB�qB�^B�9B�'B�-B�3B�'B�3B�LB�XB�qB��BBƨB��B�B�B�B��B��B�5B�TB�TB�fB�B�B�B�B��B��B��B	B	%B	+B	JB	VB	\B	hB	�B	�B	�B	#�B	"�B	"�B	#�B	$�B	$�B	&�B	)�B	+B	1'B	5?B	5?B	<jB	E�B	G�B	L�B	O�B	Q�B	R�B	R�B	S�B	S�B	T�B	W
B	YB	ZB	[#B	\)B	`BB	aHB	dZB	k�B	o�B	o�B	o�B	q�B	r�B	r�B	s�B	v�B	x�B	y�B	z�B	{�B	{�B	|�B	~�B	� B	� B	�B	�B	�B	�B	�%B	�+B	�7B	�DB	�PB	�VB	�\B	�bB	�hB	�hB	�oB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�9B	�?B	�FB	�LB	�RB	�^B	�dB	�}B	��B	B	��B	��B	��B	B	ĜB	ŢB	ƨB	ǮB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�
B	�B	�B	�#B	�)B	�/B	�/B	�/B	�/B	�BB	�TB	�`B	�fB	�fB	�fB	�`B	�fB	�fB	�mB	�mB	�sB	�sB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
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
B
B
+B

=B

=B

=B

=B

=B

=B

=B

=B

=B
DB

=B
DB
DB
DB
JB
PB
PB
VB
VB
\B
VB
VB
\B
bB
bB
hB
hB
hB
hB
hB
hB
oB
oB
uB
uB
uB
uB
uB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
 �B
"�B
&�B
2-B
9XB
=qB
B�B
J�B
M�B
P�B
[#B
^5B
dZB
gmB
k�B
o�B
u�B
y�B
{�B
�B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B
ƅB
ƄB
ƂB
ƃB
ƃB
ƄB
ƃB
ƃB
ƂB
ƀB
ƃB
ƄB
ƄB
�xB
�sB
�qB
�sB
�nB
�qB
�qB
�sB
�qB
�iB
�jB
�jB
�iB
�iB
�kB
�jB
�rB
�|B
ͭB
��B
�$B
�ZB
�xB
��B
B+�B=JB@^BT�B��B�kB��B��B��B/BϹB��Bk\Bd1B]Bt�B��Bd-Bw�BS�B8&B(�BeBPB'BB��B�bB�BиBǁB�VB�0B��B��B��B��B�5B��Bu�Bc'BL�B7B+�B(�B"�B�BGB?PB:0B.�B!�BlBFB		B
��B
��B
�mB
��B
q�B
T�B
!�B
�B	�kB	��B	�yB	�$B	��B	�B	t�B	lcB	f<B	W�B	4B	/�B	,�B	)�B	)�B	'�B	!�B	OB	B��B�B�cB�CB�B��BȐB�gB�YB�_B�dB�\B�lB�jB�kB�dB�\BƀB�jB�\B�UB�QB�JB�>B�-B�&B�B�B�B�&B�=B�fBʛBɒBʚBȊBȍBȌB�sB�mB�`B�TB�VB�GB�GB�HB�8B�B��B�B�	B��B�	B�#B�/B�GB�ZB�eB�BнB��B��B��B��B��B�	B�*B�+B�:B�SB�ZB�sB�yB��B��B��B	�B	�B	 B	B	'B	.B	;B	RB	~B	�B	#�B	"�B	"�B	#�B	$�B	$�B	&�B	)�B	*�B	0�B	5B	5B	<:B	ErB	G~B	L�B	O�B	Q�B	R�B	R�B	S�B	S�B	T�B	V�B	X�B	Y�B	Z�B	[�B	`B	aB	d(B	kRB	olB	olB	oiB	qwB	r|B	r}B	s�B	v�B	x�B	y�B	z�B	{�B	{�B	|�B	~�B	�B	�B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�"B	�&B	�/B	�5B	�5B	�:B	�FB	�GB	�KB	�RB	�dB	�fB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�&B	�.B	�FB	�TB	�YB	�SB	�TB	�QB	�ZB	�eB	�kB	�pB	�xB	�}B	�}B	ɂB	ʋB	̘B	̕B	̓B	͛B	΢B	ΣB	΢B	΢B	ϦB	ϧB	ϧB	ЭB	ѴB	ѵB	ҼB	ҼB	ҼB	��B	��B	ҽB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�/B	�0B	�-B	�(B	�-B	�,B	�4B	�3B	�<B	�=B	�4B	�BB	�JB	�QB	�SB	�TB	�RB	�WB	�aB	�_B	�dB	�_B	�_B	�mB	�vB	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
 �B
�B
�B
�B
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
�B
�B
�B

B

B

B

B

B

B

B

B

B

B

B
B
B
	B
B
B
B
B
B
$B
B
B
!B
&B
*B
,B
,B
-B
0B
,B
-B
4B
3B
<B
:B
:B
;B
9B
:B
BB
FB
KB
SB
YB
ZB
XB
XB
ZB
_B
_B
^B
^B
_B
^B
`B
`B
cB
_B
`B
gB
]B
bB
|B
�B
�B
�B
�B
�B
�B
 �B
 �B
 �G�O�B
 �B
"�B
&�B
1�B
9B
=8B
BSB
J�B
M�B
P�B
Z�B
]�B
dB
g1B
kHB
obB
u�B
y�B
{�B
��B
��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.49 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071451282016080714512820160807145128  AO  ARCAADJP                                                                    20160416091625    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20160416091625  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20160416091625  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807145128  IP                  G�O�G�O�G�O�                