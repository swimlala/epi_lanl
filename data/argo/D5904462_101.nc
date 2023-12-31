CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2016-01-28T10:16:19Z AOML 3.0 creation; 2016-08-07T21:51:26Z UW 3.1 conversion     
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
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20160128101619  20160807145126  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               eA   AO  5287_9017_101                   2C  D   APEX                            6529                            072314                          846 @בK���J1   @בL-��@0�Z�1�d˾vȴ91   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    eA   B   B   @9��@y��@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  BhffBp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B���B�  B�33B���B�  B�  B�  B���B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-y�D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dts3DyY�D��D�<�D�p D�ɚD��D�I�D�p D��fD�fD�I�D��fD�ɚD���D�VfD�vfD��D�fD�P D�|�D��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @X��@�z�@ϮA�
A'�
AG�
Ag�
A��A��A��A��A��A��A��A��B��B	��B��B��B!��B)��B1��B9��BA��BI��BQ��BY��Ba��Bj\)Bq��By��B���B���B���B���B���B���B���B���B���B���B�.B�.B���B�ǮB���B�.B�ǮB���B���B���B�ǮB���B���B���B���B�.B���B���B���B���B���B���C }qC}qC}qC}qC}qC
}qC}qC}qC}qC}qC}qC}qC}qC}qC}qC}qC }qC"}qC$}qC&}qC(}qC*}qC,}qC.}qC0}qC2}qC4}qC6}qC8}qC:}qC<}qC>}qC@}qCB}qCD}qCF}qCH}qCJ}qCL}qCN}qCP}qCR}qCT}qCV}qCX}qCZ}qC\}qC^}qC`}qCb}qCd}qCf}qCh}qCj}qCl}qCn}qCp}qCr}qCt}qCv}qCx}qCz}qC|}qC~}qC�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�1�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�1�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�D \D �\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D	\D	�\D
\D
�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D \D �\D!\D!�\D"\D"�\D#\D#�\D$\D$�\D%\D%�\D&\D&�\D'\D'�\D(\D(�\D)\D)�\D*\D*�\D+\D+�\D,\D,�\D-\D-��D.\D.�\D/\D/�\D0\D0�\D1\D1�\D2\D2�\D3\D3�\D4\D4�\D5\D5�\D6\D6�\D7\D7�\D8\D8�\D9\D9�\D:\D:�\D;\D;�\D<\D<�\D=\D=�\D>\D>�\D?\D?�\D@\D@�\DA\DA�\DB\DB�\DC\DC�\DD\DD�\DE\DE�\DF\DF�\DG\DG�\DH\DH�\DI\DI�\DJ\DJ�\DK\DK�\DL\DL�\DM\DM�\DN\DN�\DO\DO�\DP\DP�\DQ\DQ�\DR\DR�\DS\DS�\DT\DT�\DU\DU�\DV\DV�\DW\DW�\DX\DX�\DY\DY�\DZ\DZ�\D[\D[�\D\\D\�\D]\D]�\D^\D^�\D_\D_�\D`\D`�\Da\Da�\Db\Db�\Dc\Dc�\Dd\Dd�\De\De�\Df\Df�\Dg\Dg�\Dh\Dh�\Di\Di�\Dj\Dj�\Dk\Dk�\Dl\Dl�\Dm\Dm�\Dn\Dn�\Do\Do�\Dp\Dp�\Dq\Dq�\Dr\Dr�\Ds\Ds�\Dt\Dt��Dyx�D�,{D�L{D��D��HD�{D�YHD��D��D�D�YHD��D��HD��{D�fDچD��{D�&D�_�D�{D�)H111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AΛ�AΛ�AΟ�AΡ�AΛ�AΟ�AΡ�A�C�A�oA��A�"�A� �A� �A�&�A�33A�K�A�dZAΕ�AμjA���A�{A�(�A�"�A��A��A��A��A��A�{A�bA�
=A���A��yA��A��mA��`A��`A��`A��HA��;Aκ^A�x�A�1A���A�-A�"�A�Aź^A+A�jA��A���A�1'A���A�=qA�p�A��A��A��+A��uA�K�A�(�A�JA�JA�K�A���A�x�A�K�A�E�A���A��A���A�ffA�A��`A�{A~��Ay��ApVAk�Ag;dAb�RA` �A]\)AZffAW33ARVAO+AL�`AG��A@��A?��A?�A>��A<ffA<A<  A;�A;�A;A;t�A;VA:(�A6�HA5�FA5�A4��A4^5A3�FA37LA2n�A1��A0bA.=qA,ȴA,M�A+�
A+C�A*ȴA*M�A)��A);dA(��A(ZA'`BA&�yA&9XA$��A"�A ��Az�A~�A�^A�jAG�AVAt�AbNA;dA�+A�
A&�A|�AO�A�A��AQ�A��A�A�;A�`AO�A
��A	�A	�#A
�`AK�A�PAJA�wAO�A
�`A	oA�A\)A�7A�FA�h@�\)@�-@�@��T@���@�p�@�hs@�hs@�`B@�X@�V@���@� �@�"�@�^5@���@�33@�^5@��/@�t�@���@�5?@�u@�+@��#@�@��^@�`B@�@�dZ@�7L@�\)@�G�@�j@��
@�|�@�@�~�@�5?@�{@�-@�x�@�x�@��@�Z@ߍP@�S�@�@�V@��T@��#@�X@ܴ9@܋D@� �@��@��@ٺ^@�p�@�O�@�G�@�/@�V@ش9@��m@�o@�n�@�G�@Լj@ӍP@�33@�@ҸR@�{@��@Л�@�9X@��
@�C�@Ώ\@ͩ�@��@̓u@��m@��@�v�@�ff@�M�@��@��@�$�@�-@�-@�5?@�-@���@Ɂ@�?}@�V@ț�@ǝ�@� �@�z�@ȼj@�Ĝ@�1@�C�@�5?@�X@���@��`@��/@��/@���@�Ĝ@�j@�(�@��@þw@��@��#@�7L@��/@��@�9X@� �@��@��@��@��F@�S�@�;d@�+@��@���@��7@���@�r�@�r�@�bN@�Z@�I�@�9X@�(�@��@��P@�ȴ@�-@��@��@���@��^@�O�@���@���@�l�@�C�@��H@���@�n�@�M�@�=q@�E�@�=q@��@�p�@�z�@��
@�ƨ@�dZ@�+@���@�V@��@�`B@�%@���@�1'@��m@�ƨ@��@���@��P@��@�K�@��@���@��T@���@��@��P@�o@���@�n�@�@���@�7L@�%@���@��`@���@�I�@�9X@��@��;@���@��@���@�l�@�+@�+@���@�=q@���@��^@�z�@��;@���@�|�@�|�@�C�@���@���@�v�@�$�@��T@��h@�`B@�O�@�G�@�/@��@��D@��@���@�\)@�S�@�S�@�"�@���@�~�@�E�@��@��@��@��@��@���@�@��^@���@��h@��7@�G�@�V@��`@���@���@�Ĝ@��9@��@�I�@�1@��
@���@�"�@�o@��H@�-@��@��#@���@�@��@�?}@��@�%@���@���@��@��/@�Ĝ@��@���@���@��D@�r�@�  @�K�@���@���@�ff@���@���@���@�hs@���@�bN@�Q�@�Z@�bN@�bN@�j@�j@�j@�Z@�9X@�b@��
@�o@�n�@�@�/@��j@��D@�r�@�bN@�Z@�bN@�I�@� �@���@��H@�Ĝ@u��@j��@a��@VV@L�D@D�@<�@6v�@1&�@*�!@$z�@�@A�@�D@A�@1@�`@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  AΛ�AΛ�AΟ�AΡ�AΛ�AΟ�AΡ�A�C�A�oA��A�"�A� �A� �A�&�A�33A�K�A�dZAΕ�AμjA���A�{A�(�A�"�A��A��A��A��A��A�{A�bA�
=A���A��yA��A��mA��`A��`A��`A��HA��;Aκ^A�x�A�1A���A�-A�"�A�Aź^A+A�jA��A���A�1'A���A�=qA�p�A��A��A��+A��uA�K�A�(�A�JA�JA�K�A���A�x�A�K�A�E�A���A��A���A�ffA�A��`A�{A~��Ay��ApVAk�Ag;dAb�RA` �A]\)AZffAW33ARVAO+AL�`AG��A@��A?��A?�A>��A<ffA<A<  A;�A;�A;A;t�A;VA:(�A6�HA5�FA5�A4��A4^5A3�FA37LA2n�A1��A0bA.=qA,ȴA,M�A+�
A+C�A*ȴA*M�A)��A);dA(��A(ZA'`BA&�yA&9XA$��A"�A ��Az�A~�A�^A�jAG�AVAt�AbNA;dA�+A�
A&�A|�AO�A�A��AQ�A��A�A�;A�`AO�A
��A	�A	�#A
�`AK�A�PAJA�wAO�A
�`A	oA�A\)A�7A�FA�h@�\)@�-@�@��T@���@�p�@�hs@�hs@�`B@�X@�V@���@� �@�"�@�^5@���@�33@�^5@��/@�t�@���@�5?@�u@�+@��#@�@��^@�`B@�@�dZ@�7L@�\)@�G�@�j@��
@�|�@�@�~�@�5?@�{@�-@�x�@�x�@��@�Z@ߍP@�S�@�@�V@��T@��#@�X@ܴ9@܋D@� �@��@��@ٺ^@�p�@�O�@�G�@�/@�V@ش9@��m@�o@�n�@�G�@Լj@ӍP@�33@�@ҸR@�{@��@Л�@�9X@��
@�C�@Ώ\@ͩ�@��@̓u@��m@��@�v�@�ff@�M�@��@��@�$�@�-@�-@�5?@�-@���@Ɂ@�?}@�V@ț�@ǝ�@� �@�z�@ȼj@�Ĝ@�1@�C�@�5?@�X@���@��`@��/@��/@���@�Ĝ@�j@�(�@��@þw@��@��#@�7L@��/@��@�9X@� �@��@��@��@��F@�S�@�;d@�+@��@���@��7@���@�r�@�r�@�bN@�Z@�I�@�9X@�(�@��@��P@�ȴ@�-@��@��@���@��^@�O�@���@���@�l�@�C�@��H@���@�n�@�M�@�=q@�E�@�=q@��@�p�@�z�@��
@�ƨ@�dZ@�+@���@�V@��@�`B@�%@���@�1'@��m@�ƨ@��@���@��P@��@�K�@��@���@��T@���@��@��P@�o@���@�n�@�@���@�7L@�%@���@��`@���@�I�@�9X@��@��;@���@��@���@�l�@�+@�+@���@�=q@���@��^@�z�@��;@���@�|�@�|�@�C�@���@���@�v�@�$�@��T@��h@�`B@�O�@�G�@�/@��@��D@��@���@�\)@�S�@�S�@�"�@���@�~�@�E�@��@��@��@��@��@���@�@��^@���@��h@��7@�G�@�V@��`@���@���@�Ĝ@��9@��@�I�@�1@��
@���@�"�@�o@��H@�-@��@��#@���@�@��@�?}@��@�%@���@���@��@��/@�Ĝ@��@���@���@��D@�r�@�  @�K�@���@���@�ff@���@���@���@�hs@���@�bN@�Q�@�Z@�bN@�bN@�j@�j@�j@�Z@�9X@�b@��
@�o@�n�@�@�/@��j@��D@�r�@�bN@�Z@�bN@�I�G�O�@���@��H@�Ĝ@u��@j��@a��@VV@L�D@D�@<�@6v�@1&�@*�!@$z�@�@A�@�D@A�@1@�`@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	~�B	~�B	}�B	~�B	�B	� B	~�B	�DB	��B	��B	��B	��B	��B	��B	�B	�FB	�}B	��B	�;B	��B
PB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
#�B
)�B
,B
.B
.B
/B
/B
0!B
33B
9XB
=qB
?}B
@�B
hsB
ǮB�B+B_;B�%B�B��BƨBǮBɺB��B�qB�Bw�BC�B�)B�5B��BL�B
��B
�RB
��B
~�B
`BB
A�B
-B
uB
B	�B	��B	��B	�'B	��B	v�B	gmB	K�B	9XB	1'B	(�B	!�B	{B	%B��B��B�B�TB�mB�B�B��B	B	uB	�B	 �B	#�B	$�B	%�B	%�B	+B	-B	,B	,B	(�B	$�B	 �B	�B	\B	B��B�B�yB�`B�NB�;B�)B�B�B�B�
B�B�#B�B��B��B��B�5B�BB�/B�#B�B�
B��B��BȴB��B��B��B��B�B�`B��B	�B	�B	�B	VB	+B��B�B�B		7B	!�B	,B	2-B	D�B	I�B	I�B	I�B	?}B	:^B	49B	(�B	�B	bB	B	B	  B	  B��B��B��B��B��B��B��B��B��B��B	B		7B	hB	hB	�B	�B	�B	�B	$�B	7LB	7LB	6FB	6FB	5?B	33B	/B	,B	,B	0!B	49B	6FB	7LB	8RB	:^B	=qB	?}B	D�B	E�B	E�B	H�B	M�B	T�B	T�B	VB	W
B	ZB	[#B	]/B	_;B	_;B	aHB	`BB	bNB	e`B	ffB	ffB	ffB	ffB	ffB	gmB	gmB	ffB	e`B	hsB	n�B	u�B	y�B	y�B	z�B	{�B	� B	�B	�B	�+B	�bB	�{B	�{B	��B	��B	��B	��B	��B	��B	�B	�B	�3B	�?B	�LB	�LB	�XB	�jB	�wB	�qB	�jB	�jB	�^B	�qB	ĜB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�#B	�#B	�#B	�#B	�#B	�#B	�)B	�)B	�/B	�5B	�5B	�5B	�5B	�5B	�5B	�5B	�;B	�BB	�BB	�HB	�HB	�HB	�HB	�HB	�HB	�HB	�HB	�HB	�TB	�TB	�TB	�ZB	�ZB	�ZB	�ZB	�ZB	�`B	�`B	�`B	�fB	�mB	�mB	�mB	�mB	�mB	�mB	�mB	�sB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
+B
+B
+B
1B
+B
+B
+B
+B
	7B
DB
JB
JB
PB
PB
PB
PB
VB
\B
\B
\B
\B
PB
DB
JB
\B
\B
bB
hB
hB
hB
uB
{B
�B
�B
�B
#�B
)�B
0!B
8RB
@�B
I�B
P�B
W
B
\)B
`BB
cTB
hsB
m�B
r�B
u�B
y�B
}�B
�B
�%111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B	~�B	~�B	}�B	~�B	��B	�B	~�B	�'B	�rB	��B	��B	��B	��B	��B	��B	�+B	�cB	��B	�B	��B
3B
rB
�B
�B
�B
�B
�B
B
zB
�B
�B
#�B
)�B
+�B
-�B
-�B
.�B
.�B
/�B
3B
97B
=RB
?^B
@`B
hPB
ǈBqB*�B_B��B��B�oB�}BǂBɐBʓB�FB��Bw�BCjB��B�BάBL�B
��B
�&B
��B
~�B
`B
AaB
,�B
NB
�B	�pB	��B	�aB	��B	�nB	v�B	gGB	K�B	93B	1B	(�B	!�B	WB	 B��B��B�mB�.B�EB�^B�`B��B	�B	OB	jB	 �B	#�B	$�B	%�B	%�B	*�B	,�B	+�B	+�B	(�B	$�B	 �B	kB	3B	�B��B�bB�PB�9B�+B�B�B��B��B��B��B��B��B��B��B��B��B�B�B�B��B��B��BнBͩBȋB˞BʚB̨BкB��B�6B��B	gB	eB	[B	+B	�B��B�B�B		B	!�B	+�B	2B	DpB	I�B	I�B	I�B	?NB	:2B	4
B	(�B	�B	4B	�B	 �B��B��B��B��B��B��B��B��B��B��B��B��B	 �B		B	9B	:B	kB	lB	fB	mB	$�B	7B	7B	6B	6B	5B	3B	.�B	+�B	+�B	/�B	4	B	6B	7B	8!B	:/B	=?B	?MB	DlB	ErB	ErB	H�B	M�B	T�B	T�B	U�B	V�B	Y�B	Z�B	\�B	_B	_B	aB	`B	bB	e.B	f1B	f1B	f0B	f2B	f3B	g:B	g:B	f4B	e-B	h@B	ncB	u�B	y�B	y�B	z�B	{�B	�B	��B	��B	��B	�.B	�FB	�GB	�UB	�iB	�xB	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�3B	�BB	�<B	�5B	�2B	�(B	�:B	�dB	�{B	ɄB	ʉB	̕B	̕B	̕B	̗B	΢B	ΡB	ϨB	ЯB	аB	ѵB	ҼB	��B	��B	ҼB	ѶB	ѶB	ЮB	ѶB	ҼB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�"B	�!B	� B	�$B	�!B	�'B	�(B	�)B	�-B	�4B	�4B	�5B	�5B	�5B	�5B	�6B	�;B	�4B	�=B	�BB	�GB	�GB	�JB	�PB	�SB	�[B	�fB	�nB	�kB	�gB	�eB	�eB	�mB	�iB	�nB	�jB	�jB	�sB	�qB	�uB	�vB	�vB	�}B	�~B	�|B	�xB	�wB	�vB	�uB	�tB	�wB	�{B	�|B	�|B	�|B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
 �B
 �B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B

B
B
B
B
B
B
B
B
$B
"B
"B
!B
B

B
B
"B
"B
&B
/B
.B
-B
;B
DG�O�B
`B
~B
#�B
)�B
/�B
8B
@GB
I~B
P�B
V�B
[�B
`B
cB
h6B
mVB
rsB
u�B
y�B
}�B
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.49 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071451262016080714512620160807145126  AO  ARCAADJP                                                                    20160128101619    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20160128101619  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20160128101619  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807145126  IP                  G�O�G�O�G�O�                