CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2016-02-18T10:17:51Z AOML 3.0 creation; 2016-08-07T21:51:26Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20160218101751  20160807145126  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               iA   AO  5287_9017_105                   2C  D   APEX                            6529                            072314                          846 @ז��bA�1   @ז�s���@1s�E����dѩ��l�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    iA   B   B   @�33@�  A   A   A@  A`  A~ffA�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B���B���C   C�CL�C�fC��C	�fC  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2�C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CG�fCJ  CL  CN  CP  CR  CT  CV  CX�CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_y�D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dts3Dy` D�  D�L�D�s3D���D��D�\�D�� D��fD�3D�L�D��fD�� D��fD�L�D�vfD���D��fD�@ D�fD���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��G@ϮA�
A'�
AG�
Ag�
A��A��A��A��A��A��A��A��B��B	��B��B��B!��B)��B1��B9��BA��BI��BQ��BY��Ba��Bi��Bq��By��B���B���B���B���B���B���B���B���B���B�ǮB�ǮB���B���B���B���B���B���B���B�.B���B���B���B���B���B���B���B���B���B�.B���B�ǮB�ǮC }qC�C�>Cc�CJ>C
c�C}qC}qC}qC}qC}qC}qC}qC}qC}qC}qC }qC"}qC$}qC&}qC(}qC*}qC,}qC.}qC0}qC2�C4}qC6}qC8}qC:}qC<}qC>}qC@}qCB}qCD}qCF}qCHc�CJ}qCL}qCN}qCP}qCR}qCT}qCV}qCX�CZ}qC\}qC^}qC`}qCb}qCd}qCf}qCh}qCj}qCl}qCn}qCp}qCr}qCt}qCv}qCx}qCz}qC|}qC~}qC�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�K�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�D \D �\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D	\D	�\D
\D
�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D \D �\D!\D!�\D"\D"�\D#\D#�\D$\D$�\D%\D%�\D&\D&�\D'\D'�\D(\D(�\D)\D)�\D*\D*�\D+\D+�\D,\D,�\D-\D-�\D.\D.�\D/\D/�\D0\D0�\D1\D1�\D2\D2�\D3\D3�\D4\D4�\D5\D5�\D6\D6�\D7\D7�\D8\D8�\D9\D9�\D:\D:�\D;\D;�\D<\D<�\D=\D=�\D>\D>�\D?\D?�\D@\D@�\DA\DA�\DB\DB�\DC\DC�\DD\DD�\DE\DE�\DF\DF�\DG\DG�\DH\DH�\DI\DI�\DJ\DJ�\DK\DK�\DL\DL�\DM\DM�\DN\DN�\DO\DO�\DP\DP�\DQ\DQ�\DR\DR�\DS\DS�\DT\DT�\DU\DU�\DV\DV�\DW\DW�\DX\DX�\DY\DY�\DZ\DZ�\D[\D[�\D\\D\�\D]\D]�\D^\D^�\D_\D_��D`\D`�\Da\Da�\Db\Db�\Dc\Dc�\Dd\Dd�\De\De�\Df\Df�\Dg\Dg�\Dh\Dh�\Di\Di�\Dj\Dj�\Dk\Dk�\Dl\Dl�\Dm\Dm�\Dn\Dn�\Do\Do�\Dp\Dp�\Dq\Dq�\Dr\Dr�\Ds\Ds�\Dt\Dt��Dy\D��D�\{D���D��{D�{D�l{D���D��D�"�D�\{D��D�ϮD�D�\{DچD��{D�D�O�D�D��{11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A���A���A���A���A���A���A�  A�A�A�%A�1A�A�A�A�%A�1A�A�A���A���A���A���A��A��A��A��A��A��yA��yA��A��A���A���A���A��A��A��A���A���A���A�A�A�1A�{A��A��A��A��A��A��A� �A�"�A�"�A�$�A�$�A� �A�1A�A�A�Q�A�C�A��A�ZA���A��yA��mA��A�E�A�~�A�/A�|�A�K�A���A��HA��7A�9XA��HA��\A���A�1A�-A�A�9XA�n�A���A��FA�1'A�A���A��HA���A�Q�A��A���A���A��A���A��A��jA�`BA�33A��A�|�A~��A{��Az1'Au7LAq7LApĜAln�Aj�Aj~�Af5?A`��A_��A]��A\��A\A�AY�AVJAT �AQ�AL^5AI��AH5?AE�
AD�uAC+A?
=A<��A:�A9?}A7�A4��A2{A1;dA0jA/�A.�uA-/A,�A+��A+��A+/A*�/A*��A)C�A(ȴA(�A'��A&�!A%VA$E�A#+A"�jA"��A"Q�A!�#A!�-A!/A �A A�A�wA�uA1A`BAVAl�AjAO�A�`A��A�9A�\AbNA��A�TA{AZA"�AhsA\)AC�A;dA7LA+A"�A��A�/A��A��A��A�jAĜA�jA��A��AVAZA9XA�AK�A�A�HA�jA~�An�AVA �A��AS�A+A�yA�uA�PAXAO�A7LA�DA{A��A��A��AbNA�A�-A�AC�AA��AjA
=A^5A�A�-A7LA
��A
�A
v�A
ZA
 �A
A	�
A	l�A	"�A	
=A��A9XA�mA�hA;dA�!A �Ax�Az�A�A�A�A�A"�A�+A~�Ar�AA�A�A�AA|�A Ĝ@��@��@�~�@�v�@�^5@��@��j@��@��@��T@��@�I�@�C�@�X@�ȴ@�b@�X@�A�@���@�F@���@睲@�@��@�bN@�S�@���@���@�+@�M�@�ff@��@�p�@�Z@�\)@�+@߮@�C�@ߕ�@� �@�`B@�h@�Ĝ@�1@ޟ�@���@�@���@�  @�|�@�t�@�
=@ڟ�@���@ٙ�@�V@�n�@�p�@ԣ�@��
@�33@���@���@�~�@�z�@��@�V@�b@�(�@���@�C�@�o@��H@ʧ�@�=q@��T@�{@ɉ7@�Ĝ@ȴ9@ț�@�9X@Ǖ�@�;d@�@�ȴ@�^5@�^5@�ff@�v�@�ff@��#@�hs@�p�@�x�@ŉ7@ź^@ź^@Ł@�/@�%@���@���@���@ļj@ă@�bN@�I�@Ý�@�{@��h@�?}@���@�Q�@�b@��m@��F@���@��@���@�`B@�V@�Ĝ@�r�@��m@��@���@�b@�;d@��@�
=@���@�r�@� �@���@�ƨ@�t�@�33@�o@�@���@�^5@�E�@�5?@�@��#@���@�G�@��D@�r�@�j@�bN@�Z@�I�@�9X@�b@�dZ@�;d@�@���@�E�@�-@���@���@���@�I�@� �@�b@�  @���@��
@���@�ƨ@��w@��F@���@���@�K�@�n�@���@�x�@�%@���@��j@��u@��@�bN@�9X@�|�@�+@�@���@�V@�{@���@��h@�7L@���@��@��D@�I�@��@�+@�v�@�=q@�@���@���@�X@��@��u@�z�@��;@�|�@�C�@�~�@���@���@��-@��@��@��@���@���@� �@s��@iX@\��@V$�@M@F��@@��@:n�@4z�@,(�@$�@ �@��@@o11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   A���A���A���A���A���A���A���A�  A�A�A�%A�1A�A�A�A�%A�1A�A�A���A���A���A���A��A��A��A��A��A��yA��yA��A��A���A���A���A��A��A��A���A���A���A�A�A�1A�{A��A��A��A��A��A��A� �A�"�A�"�A�$�A�$�A� �A�1A�A�A�Q�A�C�A��A�ZA���A��yA��mA��A�E�A�~�A�/A�|�A�K�A���A��HA��7A�9XA��HA��\A���A�1A�-A�A�9XA�n�A���A��FA�1'A�A���A��HA���A�Q�A��A���A���A��A���A��A��jA�`BA�33A��A�|�A~��A{��Az1'Au7LAq7LApĜAln�Aj�Aj~�Af5?A`��A_��A]��A\��A\A�AY�AVJAT �AQ�AL^5AI��AH5?AE�
AD�uAC+A?
=A<��A:�A9?}A7�A4��A2{A1;dA0jA/�A.�uA-/A,�A+��A+��A+/A*�/A*��A)C�A(ȴA(�A'��A&�!A%VA$E�A#+A"�jA"��A"Q�A!�#A!�-A!/A �A A�A�wA�uA1A`BAVAl�AjAO�A�`A��A�9A�\AbNA��A�TA{AZA"�AhsA\)AC�A;dA7LA+A"�A��A�/A��A��A��A�jAĜA�jA��A��AVAZA9XA�AK�A�A�HA�jA~�An�AVA �A��AS�A+A�yA�uA�PAXAO�A7LA�DA{A��A��A��AbNA�A�-A�AC�AA��AjA
=A^5A�A�-A7LA
��A
�A
v�A
ZA
 �A
A	�
A	l�A	"�A	
=A��A9XA�mA�hA;dA�!A �Ax�Az�A�A�A�A�A"�A�+A~�Ar�AA�A�A�AA|�A Ĝ@��@��@�~�@�v�@�^5@��@��j@��@��@��T@��@�I�@�C�@�X@�ȴ@�b@�X@�A�@���@�F@���@睲@�@��@�bN@�S�@���@���@�+@�M�@�ff@��@�p�@�Z@�\)@�+@߮@�C�@ߕ�@� �@�`B@�h@�Ĝ@�1@ޟ�@���@�@���@�  @�|�@�t�@�
=@ڟ�@���@ٙ�@�V@�n�@�p�@ԣ�@��
@�33@���@���@�~�@�z�@��@�V@�b@�(�@���@�C�@�o@��H@ʧ�@�=q@��T@�{@ɉ7@�Ĝ@ȴ9@ț�@�9X@Ǖ�@�;d@�@�ȴ@�^5@�^5@�ff@�v�@�ff@��#@�hs@�p�@�x�@ŉ7@ź^@ź^@Ł@�/@�%@���@���@���@ļj@ă@�bN@�I�@Ý�@�{@��h@�?}@���@�Q�@�b@��m@��F@���@��@���@�`B@�V@�Ĝ@�r�@��m@��@���@�b@�;d@��@�
=@���@�r�@� �@���@�ƨ@�t�@�33@�o@�@���@�^5@�E�@�5?@�@��#@���@�G�@��D@�r�@�j@�bN@�Z@�I�@�9X@�b@�dZ@�;d@�@���@�E�@�-@���@���@���@�I�@� �@�b@�  @���@��
@���@�ƨ@��w@��F@���@���@�K�@�n�@���@�x�@�%@���@��j@��u@��@�bN@�9X@�|�@�+@�@���@�V@�{@���@��h@�7L@���@��@��D@�I�@��@�+@�v�@�=q@�@���@���@�X@��@��u@�z�@��;@�|�@�C�@�~�@���@���G�O�@��@��@��@���@���@� �@s��@iX@\��@V$�@M@F��@@��@:n�@4z�@,(�@$�@ �@��@@o11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
~�B
~�B
~�B
� B
~�B
� B
� B
~�B
~�B
~�B
~�B
~�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
~�B
~�B
� B
� B
� B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�%B
�%B
�+B
�+B
�7B
�7B
�=B
�\B
�hB
�oB
�{B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B0!Bl�B�
B/BQ�B�B�\B��B��B�{B��B��B��B�jB�'B�^B�jB��B��B�hB�Bn�BcTB]/BVBB�B"�B{B+B��B�mB�^B�JB`BBJB
�;B
��B
�B
��B
k�B
XB
Q�B
L�B
C�B
(�B	��B	�fB	��B	�-B	��B	�oB	~�B	w�B	s�B	ZB	E�B	A�B	9XB	5?B	0!B	%�B	uB	1B��B�B�TB�/B�#B�B�
B�B�BB�fB�mB�B	B	1B		7B	JB	hB	'�B	8RB	>wB	E�B	E�B	F�B	J�B	O�B	`BB	e`B	gmB	hsB	dZB	]/B	aHB	dZB	gmB	hsB	jB	jB	k�B	n�B	s�B	w�B	z�B	� B	�B	�B	�%B	�1B	�DB	�PB	�\B	�bB	�bB	�bB	��B	��B	��B	�B	�qB	�B	�fB	�yB	�B	�B	�B	��B	��B
B
+B
+B
1B
	7B

=B
DB
JB
JB
PB
\B
�B
�B
"�B
'�B
(�B
(�B
(�B
&�B
%�B
%�B
'�B
(�B
+B
.B
/B
.B
1'B
0!B
0!B
/B
/B
/B
/B
.B
.B
.B
.B
.B
.B
.B
-B
-B
,B
)�B
)�B
)�B
)�B
(�B
'�B
'�B
&�B
&�B
%�B
%�B
%�B
%�B
&�B
%�B
%�B
$�B
"�B
�B
�B
�B
�B
�B
hB

=B
%B

=B
oB
oB
oB
oB
oB
oB
oB
bB
bB
VB
DB
+B
1B

=B

=B

=B
1B
B	��B	�B	�yB	�fB	�ZB	�HB	�B	��B	��B	B	�wB	�wB	�}B	��B	��B	�}B	�wB	�qB	��B	��B	ÖB	ÖB	ȴB	��B	��B	��B	��B	��B	�B	�/B	�BB	�ZB	�sB	�B	�B	�B	�B	�yB	�B	�B	�B	�B	�B	�B	�B	�yB	�`B	�`B	�TB	�)B	�B	��B	��B	��B	��B	��B	��B	ǮB	ȴB	ȴB	ɺB	��B	��B	��B	��B	�
B	�B	�B	�B	�#B	�#B	�B	�B	�B	�
B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�)B	�/B	�5B	�;B	�NB	�ZB	�fB	�`B	�`B	�fB	�fB	�fB	�fB	�fB	�fB	�`B	�ZB	�NB	�HB	�BB	�BB	�BB	�NB	�NB	�ZB	�sB	�yB	�B	�B	�B	�B	�B	�yB	�fB	�ZB	�HB	�;B	�;B	�;B	�)B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�#B	�#B	�)B	�/B	�5B	�HB	�HB	�HB	�NB	�TB	�TB	�TB	�TB	�ZB	�`B	�`B	�`B	�fB	�fB	�fB	�fB	�fB	�fB	�fB	�fB	�fB	�`B	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
uB
uB
�B
�B
$�B
(�B
1'B
:^B
A�B
G�B
L�B
O�B
T�B
YB
_;B
hsB
l�B
o�B
u�B
w�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B
~�B
~�B
~�B
�B
~�B
�B
�B
~�B
~�B
~�B
~�B
~�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
~�B
~�B
�B
�B
�B
�B
�B
��B
��B
��B
��B
��B
��B
��B
�B
� B
�	B
�
B
�B
�B
�B
�8B
�EB
�LB
�VB
�]B
�]B
�cB
�cB
�eB
�cB
�eB
�jB
�qB
��B/�BlbB��B.�BQ�B��B�5B�pB��B�QB�ZB�\B��B�?B��B�7B�?B��B��B�;B��BnmBc$B]BU�BBaB"�BPB�B��B�@B�/B�B`BB
�B
�[B
��B
��B
kWB
W�B
Q�B
L�B
CoB
(�B	��B	�=B	��B	�B	�mB	�HB	~�B	w�B	s�B	Y�B	E~B	AcB	91B	5B	/�B	%�B	RB	B��B�iB�1B�B� B��B��B��B�B�?B�JB�jB	�B		B		B	"B	@B	'�B	8&B	>MB	ExB	EyB	F~B	J�B	O�B	`B	e2B	g?B	hIB	d,B	]B	aB	d.B	g@B	hGB	jSB	jUB	kWB	njB	s�B	w�B	z�B	�B	��B	��B	��B	�B	�B	�$B	�,B	�6B	�5B	�4B	�XB	��B	��B	��B	�=B	��B	�5B	�HB	�TB	�fB	�|B	��B	��B
�B
�B
�B
 B
	B

	B
B
B
B
B
)B
TB
jB
"�B
'�B
(�B
(�B
(�B
&�B
%�B
%�B
'�B
(�B
*�B
-�B
.�B
-�B
0�B
/�B
/�B
.�B
.�B
.�B
.�B
-�B
-�B
-�B
-�B
-�B
-�B
-�B
,�B
,�B
+�B
)�B
)�B
)�B
)�B
(�B
'�B
'�B
&�B
&�B
%�B
%�B
%�B
%�B
&�B
%�B
%�B
$�B
"�B
�B
}B
rB
iB
KB
4B

	B
�B

B
:B
:B
7B
7B
8B
7B
:B
-B
/B
"B
B
�B
�B

B

B

	B
�B
�B	��B	�IB	�FB	�1B	�'B	�B	��B	��B	̘B	�[B	�EB	�FB	�GB	�QB	�TB	�IB	�CB	�;B	�OB	�VB	�aB	�cB	�~B	ˑB	̙B	˒B	ʍB	ϪB	��B	��B	�B	�%B	�>B	�mB	�wB	�fB	�YB	�EB	�IB	�ZB	�WB	�IB	�SB	�[B	�PB	�DB	�,B	�*B	�B	��B	��B	��B	ѶB	ϫB	ΤB	ϨB	ϨB	�zB	�}B	�~B	ɅB	ʊB	͞B	ҽB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ҼB	ҽB	ҽB	ӿB	��B	��B	��B	��B	��B	� B	�B	�B	�$B	�0B	�)B	�*B	�.B	�0B	�/B	�/B	�0B	�-B	�'B	�$B	�B	�B	�B	�	B	�B	�B	�B	�%B	�=B	�BB	�GB	�HB	�PB	�OB	�JB	�BB	�.B	�#B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�"B	�)B	�(B	�(B	�/B	�0B	�0B	�.B	�,B	�0B	�0B	�1B	�-B	�(B	�7B	�DB	�FB	�MB	�KB	�TB	�TB	�SB	�TB	�SB	�_B	�gB	�fB	�eB	�jB	�pB	�rB	�xB	�wB	�~B	�B	�}B	�~B	�~B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��G�O�B
�B
:B
<B
XB
zB
$�B
(�B
0�B
:#B
AOB
GuB
L�B
O�B
T�B
X�B
^�B
h5B
lPB
obB
u�B
w�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.49 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071451262016080714512620160807145126  AO  ARCAADJP                                                                    20160218101751    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20160218101751  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20160218101751  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807145126  IP                  G�O�G�O�G�O�                