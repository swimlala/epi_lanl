CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2017-06-01T17:02:40Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         (Matthew Alkire, University of Washington      @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
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
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20170601170240  20190405100803  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  5287                            2C  D   APEX                            6529                            072314                          846 @��m�1   @����r@-��/���d|�hs1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  BpffBx  B�  B�  B�  B�  B�  B�  B���B���B���B�  B�  B�  B�ffB�ffB�ffB�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C�C33C�fC�fC  C   C"  C$  C&  C(  C*  C,  C.�C0�C2�C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?y�D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di�fDj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� DtٚDy� D��D�S3D��3D���D�fD�9�D�� D�� D��D�<�D���DǶfD�	�D�I�D�y�D�Y�D�3D�I�D� D�6f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@ϮA�
A'�
AG�
Ag�
A��A��A��A��A��A��A��A��B��B	��B��B��B!��B)��B1��B9��BA��BI��BQ��BY��Ba��Bi��Br\)By��B���B���B���B���B���B���B�ǮB��{B�ǮB���B���B���B�aGB�aGB�aGB���B�ǮB���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C }qC}qC}qC}qC}qC
}qC}qC}qC}qC}qC}qC�C��Cc�Cc�C}qC }qC"}qC$}qC&}qC(}qC*}qC,}qC.�C0�C2�C4}qC6}qC8}qC:}qC<}qC>}qC@}qCB}qCD}qCF}qCH}qCJ}qCL}qCN}qCP}qCR}qCT}qCV}qCX}qCZ}qC\}qC^}qC`}qCb}qCd}qCf}qCh}qCj}qCl}qCn}qCp}qCr}qCt}qCv}qCx}qCz}qC|}qC~}qC�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�D \D �\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D	\D	�\D
\D
�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D \D �\D!\D!�\D"\D"�\D#\D#�\D$\D$�\D%\D%�\D&\D&�\D'\D'�\D(\D(�\D)\D)�\D*\D*�\D+\D+�\D,\D,�\D-\D-�\D.\D.�\D/\D/�\D0\D0�\D1\D1�\D2\D2�\D3\D3�\D4\D4�\D5\D5�\D6\D6�\D7\D7�\D8\D8�\D9\D9�\D:\D:�\D;\D;�\D<\D<�\D=\D=�\D>\D>�\D?\D?��D@\D@�\DA\DA�\DB\DB�\DC\DC�\DD\DD�\DE\DE�\DF\DF�\DG\DG�\DH\DH�\DI\DI�\DJ\DJ�\DK\DK�\DL\DL�\DM\DM�\DN\DN�\DO\DO�\DP\DP�\DQ\DQ�\DR\DR�\DS\DS�\DT\DT�\DU\DU�\DV\DV�\DW\DW�\DX\DX�\DY\DY�\DZ\DZ�\D[\D[�\D\\D\�\D]\D]�\D^\D^�\D_\D_�\D`\D`�\Da\Da�\Db\Db�\Dc\Dc�\Dd\Dd�\De\De�\Df\Df�\Dg\Dg�\Dh\Dh�\Di\Di��Dj\Dj�\Dk\Dk�\Dl\Dl�\Dm\Dm�\Dn\Dn�\Do\Do�\Dp\Dp�\Dq\Dq�\Dr\Dr�\Ds\Ds�\Dt\Dt�\Dt��Dy�\D�{D�b�D���D��{D�D�IHD���D�ϮD�,{D�L{D��{D��D�HD�YHDډHD�iHD��D�YHD�D�F111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A� �A� �A� �A��A��A� �A�"�A�"�A�$�A�$�A�&�A�(�A�&�A��A��A��A��A�oA��A��A�oA�bA�oA�VA�A��yAѩ�A�VA�7LA��A��`A���A�AЕ�A���A�JA�l�A�
=A�I�A�S�Aĝ�A�;dA���AËDA��`A�VA�x�A��RA�M�A��PA��jA���A��A��;A�
=A�(�A���A���A��\A�E�A�ffA��wA��hA��A��DA��FA���A�p�A���A��A���A�
=A��hA��-A�~�A�A��-A��+A��
A��hA�9XAzA�Aw��AvZAt^5Ar�Aq&�AoƨAn1Ae�AcC�A^�\AZz�AWXAS�
AR^5APZAO�AO
=AO&�AN��AL��AKl�AI��AH�yAGAE�PAC
=AAt�AA%A@n�A?G�A>Q�A=33A;�mA:$�A9oA8Q�A7��A5��A3&�A1�^A1��A1&�A0��A0=qA/�
A/�FA.~�A-S�A,��A,VA,9XA+��A+t�A*1'A(�HA(1A'�7A&�uA&bA$��A$1A#K�A"�yA"r�A!/A!;dA"n�A#�A"�A"z�A"=qA!�wA!G�A!?}A �DAĜA|�A��A&�A��AjAM�AI�A�#A��A�AVAjAJA
=A9XA�A�^A��A?}AffA&�A�/A�A�FA�AVA��AhsA&�A�/A��AZA�;A`BA�RAn�A1'A�;AG�A��A�A�;A��A�#A��A��A-A�-A
�A
n�A	�;A	\)A	�A�HAZAƨA�A�AQ�A��A�A�/A  A��A�AA��A5?A��A\)A�A Z@�|�@�n�@�{@�@�G�@�j@��@��@���@���@�V@�1'@��F@�\)@��@��\@�-@��-@��j@�1'@�!@�A�@@�ȴ@�`B@�V@�@�@�bN@��@�@�33@�5?@�h@�?}@�&�@��/@�u@��m@�=q@��@�1@㕁@�;d@�M�@��@��u@�I�@��;@�;d@�M�@ݑh@�O�@�7L@�/@�V@���@ܓu@�j@���@�K�@�~�@�@�@���@�|�@�
=@��@և+@�@ԓu@�K�@��H@�v�@�x�@Гu@�I�@�ƨ@ύP@��;@ύP@�l�@·+@�x�@�?}@̓u@�  @��H@Ɂ@�&�@�/@���@�1'@�A�@ȓu@�/@�G�@���@�z�@�1@�\)@Ł@ă@��;@���@°!@�v�@�`B@�(�@��F@��P@�;d@�@��h@�O�@��j@��@���@�33@�E�@��@��-@�`B@�?}@�?}@�?}@�G�@�p�@�hs@��@��j@���@���@�@�o@�@��@��@���@�33@��@�~�@�5?@�{@��@��T@�@�X@���@�Z@�1'@��@���@���@���@�p�@�hs@�X@��@���@�I�@��;@��@�t�@�K�@��@�M�@�J@��#@��-@�?}@�V@��/@���@�Z@��m@��P@�
=@�n�@�$�@���@�`B@��@��@���@�9X@��@��@�"�@�ȴ@�v�@�5?@�@��7@��@�/@��`@��9@���@�r�@�1'@��m@��@��P@��@���@�{@�x�@��@�j@�Z@�(�@���@�|�@���@�=q@��@��-@�G�@��/@���@�A�@�1'@� �@��m@���@�
=@��!@�v�@�-@��T@��@��@���@�hs@�%@��u@�(�@��
@���@�l�@�S�@�"�@���@���@�v�@�E�@��@�@��@���@���@�p�@�7L@�&�@��/@�r�@�I�@��@�~�@��-@�7L@��F@|1@s33@i�#@a�7@VV@J^5@C@<1@7;d@1&�@*=q@"-@��@�@=q@��@ƨ111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A� �A� �A� �A��A��A� �A�"�A�"�A�$�A�$�A�&�A�(�A�&�A��A��A��A��A�oA��A��A�oA�bA�oA�VA�A��yAѩ�A�VA�7LA��A��`A���A�AЕ�A���A�JA�l�A�
=A�I�A�S�Aĝ�A�;dA���AËDA��`A�VA�x�A��RA�M�A��PA��jA���A��A��;A�
=A�(�A���A���A��\A�E�A�ffA��wA��hA��A��DA��FA���A�p�A���A��A���A�
=A��hA��-A�~�A�A��-A��+A��
A��hA�9XAzA�Aw��AvZAt^5Ar�Aq&�AoƨAn1Ae�AcC�A^�\AZz�AWXAS�
AR^5APZAO�AO
=AO&�AN��AL��AKl�AI��AH�yAGAE�PAC
=AAt�AA%A@n�A?G�A>Q�A=33A;�mA:$�A9oA8Q�A7��A5��A3&�A1�^A1��A1&�A0��A0=qA/�
A/�FA.~�A-S�A,��A,VA,9XA+��A+t�A*1'A(�HA(1A'�7A&�uA&bA$��A$1A#K�A"�yA"r�A!/A!;dA"n�A#�A"�A"z�A"=qA!�wA!G�A!?}A �DAĜA|�A��A&�A��AjAM�AI�A�#A��A�AVAjAJA
=A9XA�A�^A��A?}AffA&�A�/A�A�FA�AVA��AhsA&�A�/A��AZA�;A`BA�RAn�A1'A�;AG�A��A�A�;A��A�#A��A��A-A�-A
�A
n�A	�;A	\)A	�A�HAZAƨA�A�AQ�A��A�A�/A  A��A�AA��A5?A��A\)A�A Z@�|�@�n�@�{@�@�G�@�j@��@��@���@���@�V@�1'@��F@�\)@��@��\@�-@��-@��j@�1'@�!@�A�@@�ȴ@�`B@�V@�@�@�bN@��@�@�33@�5?@�h@�?}@�&�@��/@�u@��m@�=q@��@�1@㕁@�;d@�M�@��@��u@�I�@��;@�;d@�M�@ݑh@�O�@�7L@�/@�V@���@ܓu@�j@���@�K�@�~�@�@�@���@�|�@�
=@��@և+@�@ԓu@�K�@��H@�v�@�x�@Гu@�I�@�ƨ@ύP@��;@ύP@�l�@·+@�x�@�?}@̓u@�  @��H@Ɂ@�&�@�/@���@�1'@�A�@ȓu@�/@�G�@���@�z�@�1@�\)@Ł@ă@��;@���@°!@�v�@�`B@�(�@��F@��P@�;d@�@��h@�O�@��j@��@���@�33@�E�@��@��-@�`B@�?}@�?}@�?}@�G�@�p�@�hs@��@��j@���@���@�@�o@�@��@��@���@�33@��@�~�@�5?@�{@��@��T@�@�X@���@�Z@�1'@��@���@���@���@�p�@�hs@�X@��@���@�I�@��;@��@�t�@�K�@��@�M�@�J@��#@��-@�?}@�V@��/@���@�Z@��m@��P@�
=@�n�@�$�@���@�`B@��@��@���@�9X@��@��@�"�@�ȴ@�v�@�5?@�@��7@��@�/@��`@��9@���@�r�@�1'@��m@��@��P@��@���@�{@�x�@��@�j@�Z@�(�@���@�|�@���@�=q@��@��-@�G�@��/@���@�A�@�1'@� �@��m@���@�
=@��!@�v�@�-@��T@��@��@���@�hs@�%@��u@�(�@��
@���@�l�@�S�@�"�@���@���@�v�@�E�@��@�@��@���@���@�p�@�7L@�&�@��/@�r�@�I�G�O�@�~�@��-@�7L@��F@|1@s33@i�#@a�7@VV@J^5@C@<1@7;d@1&�@*=q@"-@��@�@=q@��@ƨ111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	R�B	R�B	R�B	R�B	R�B	R�B	R�B	R�B	R�B	R�B	R�B	R�B	S�B	S�B	S�B	S�B	S�B	VB	T�B	T�B	VB	VB	VB	VB	W
B	XB	]/B	bNB	cTB	cTB	dZB	e`B	dZB	cTB	y�B	��B	��B
%B
E�B
��B
�sB
��B	7B�BM�B�B�{B��B��B��B��B��B�
B��B��B��B��BÖB��BÖB��B��B�B��B�B�uBr�BXBVBP�B2-BB
�B
��B
��B
n�B
�B	�#B	��B	��B	|�B	gmB	_;B	VB	J�B	e`B	l�B	jB	aHB	7LB	,B	hB��B��B�B�B�B�B��B	
=B	VB	JB	+B	  B��B��B�B�fB�TB�HB�5B�#B�#B�#B�B�
B��B��B��BĜB�LB�RB�)B��B	B	%B	+B	+B	�B	9XB	E�B	M�B	R�B	_;B	t�B	�B	�B	�\B	�PB	�+B	�B	�B	�B	�B	�B	�JB	�DB	��B	��B	�
B	�HB	�fB	�B	�B	�B	�B	�B	�B	�HB	��B	��B	ɺB	ɺB	ȴB	��B	��B	�
B	�fB	�B	��B	��B	��B	��B	��B	��B	��B
+B
B	��B	��B	��B
  B
�B
�B
�B
�B
�B
�B
�B
{B
{B
oB
oB
oB
{B
�B
�B
�B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
bB
JB
	7B

=B
\B
hB
\B
JB
DB

=B
1B
B
B
B
B
B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�yB	�yB	�yB	�yB	�mB	�`B	�`B	�fB	�TB	�HB	�NB	�NB	�`B	�B	�B	�B	�yB	�fB	�fB	�fB	�ZB	�HB	�;B	�BB	�NB	�`B	�fB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�sB	�mB	�`B	�`B	�`B	�`B	�fB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
%B
%B
+B
+B
+B
+B
1B
1B
1B
1B
1B
	7B
	7B

=B
DB
DB
DB
JB
JB
JB
JB
PB
PB
PB
PB
PB
JB
JB
PB
VB
\B
\B
bB
bB
bB
bB
bB
bB
bB
bB
bB
\B
VB
JB
DB
JB
PB
PB
PB
PB
VB
\B
\B
hB
uB
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
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
#�B
)�B
.B
5?B
:^B
A�B
F�B
J�B
N�B
T�B
YB
]/B
bNB
gmB
l�B
q�B
w�B
{�B
~�B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B	R�B	R�B	R�B	R�B	R�B	R�B	R�B	R�B	R�B	R�B	R�B	R�B	S�B	S�B	S�B	S�B	S�B	U�B	T�B	T�B	U�B	U�B	U�B	U�B	V�B	W�B	]B	b'B	c-B	c.B	d3B	e9B	d7B	c,B	y�B	�yB	��B
�B
EyB
�oB
�IB
��B	BwBM�B��B�RB�\B�nB�zB��BͨB��B��BͧB��B̢B�iB�ZB�hB�\B�XB��B��B��B�IBr�BW�BU�BP�B1�B�B
�PB
ʔB
��B
nkB
}B	��B	ʒB	��B	|�B	g:B	_B	U�B	J�B	e,B	lXB	jLB	aB	7B	+�B	3B��B��B�|B�gB�TB�IB��B	
B	"B	B	�B��B��B��B�nB�2B�B�B��B��B��B��B��B��B��BЭB̖B�eB�B�B��B��B	�B	�B	�B	�B	cB	9B	EhB	M�B	R�B	_B	t�B	��B	��B	�$B	�B	��B	��B	��B	��B	��B	��B	�B	�B	�}B	�JB	��B	�B	�,B	�HB	�WB	�_B	�sB	�kB	�EB	�B	ѳB	͘B	ɀB	ɁB	�yB	ˎB	ΞB	��B	�/B	�]B	��B	��B	��B	��B	��B	��B	��B
�B
 �B	��B	��B	��B	��B
MB
YB
PB
KB
MB
HB
EB
CB
@B
1B
3B
4B
?B
DB
KB
FB
>B
SB
jB
oB
jB
oB
lB
eB
dB
jB
kB
fB
bB
jB
rB
kB
dB
XB
QB
MB
DB
&B
B
�B

B
!B
,B
B
B
B

B
�B
�B
�B
�B
�B
 �B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�xB	�sB	�qB	�uB	�rB	�sB	�sB	�xB	�xB	�{B	�zB	�xB	�~B	�yB	�xB	�wB	�sB	�mB	�mB	�rB	�kB	�lB	�dB	�aB	�aB	�\B	�WB	�ZB	�UB	�SB	�QB	�RB	�UB	�RB	�SB	�TB	�NB	�[B	�`B	�[B	�YB	�SB	�IB	�AB	�9B	�9B	�9B	�;B	�/B	� B	�$B	�(B	�B	�B	�B	�B	�#B	�EB	�@B	�CB	�:B	�(B	�&B	�)B	�B	�
B	��B	�B	�B	�B	�(B	�<B	�LB	�kB	�rB	�qB	�sB	�jB	�eB	�HB	�@B	�NB	�MB	�LB	�IB	�:B	�>B	�@B	�KB	�YB	�QB	�KB	�HB	�AB	�6B	�1B	�B	�B	�!B	�"B	�&B	�;B	�=B	�;B	�AB	�LB	�_B	�fB	�ZB	�WB	�fB	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
�B
�B
 �B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B

B
B
B
B
B
B
B

B
B
B
B
B
B
B

B
B
B
B
B
&B
!B
!B
#B
$B
 B
"B
"B
"B
B
B

B
B
B
B
B
B
B
B
B
B
(B
6B
-B
.B
/B
4B
8B
4B
6B
3B
4B
5B
<B
@B
@B
HB
IB
MB
FB
MB
MB
MB
NB
KB
KB
GB
MB
NB
MB
SB
XB
ZB
XB
[B
aB
aB
aB
`B
_B
aB
aG�O�B
yB
#�B
)�B
-�B
4�B
: B
AIB
FjB
J�B
N�B
T�B
X�B
\�B
bB
g.B
lMB
qlB
w�B
{�B
~�B
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.49 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             201904051008032019040510080320190405100803  AO  ARCAADJP                                                                    20170601170240    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20170601170240  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20170601170240  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190405100803  IP                  G�O�G�O�G�O�                