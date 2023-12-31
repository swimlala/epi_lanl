CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-07-21T23:05:12Z AOML 3.0 creation; 2016-05-31T19:14:26Z UW 3.1 conversion     
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
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20140721230512  20160531121426  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  4051_7090_012                   2C  D   APEX                            5368                            041511                          846 @�P5�W�1   @�P6ο�@3���n��d"���m1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @@  @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�33B�ffB�ffB�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<�C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Cg�fCj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#fD#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  DtffDy��D���D�FfD�y�D���D��D�I�D�|�D�� D�fD�P D��fD�ɚD���D�@ Dڙ�D��fD�fD�L�D��D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @=p�@}p�@��R@��RA\)A?\)A_\)A\)A��A��A��A��AϮA߮A�A��B�
B�
B�
B�
B'�
B/�
B7�
B?�
BG�
BO�
BW�
B_�
Bg�
Bo�
Bw�
B��B�Q�B�Q�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C<]C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg�)Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D }qD �qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD	}qD	�qD
}qD
�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD }qD �qD!}qD!�qD"}qD#�D#}qD#�qD$}qD$�qD%}qD%�qD&}qD&�qD'}qD'�qD(}qD(�qD)}qD)�qD*}qD*�qD+}qD+�qD,}qD,�qD-}qD-�qD.}qD.�qD/}qD/�qD0}qD0�qD1}qD1�qD2}qD2�qD3}qD3�qD4}qD4�qD5}qD5�qD6}qD6�qD7}qD7�qD8}qD8�qD9}qD9�qD:}qD:�qD;}qD;�qD<}qD<�qD=}qD=�qD>}qD>�qD?}qD?�qD@}qD@�qDA}qDA�qDB}qDB�qDC}qDC�qDD}qDD�qDE}qDE�qDF}qDF�qDG}qDG�qDH}qDH�qDI}qDI�qDJ}qDJ�qDK}qDK�qDL}qDL�qDM}qDM�qDN}qDN�qDO}qDO�qDP}qDP�qDQ}qDQ�qDR}qDR�qDS}qDS�qDT}qDT�qDU}qDU�qDV}qDV�qDW}qDW�qDX}qDX�qDY}qDY�qDZ}qDZ�qD[}qD[�qD\}qD\�qD]}qD]�qD^}qD^�qD_}qD_�qD`}qD`�qDa}qDa�qDb}qDb�qDc}qDc�qDd}qDd�qDe}qDe�qDf}qDf�qDg}qDg�qDh}qDh�qDi}qDi�qDj}qDj�qDk}qDk�qDl}qDl�qDm}qDm�qDn}qDn�qDo}qDo�qDp}qDp�qDq}qDq�qDr}qDr�qDs}qDs�qDtc�Dy�D���D�ED�xRD���D�RD�HRD�{�D�θD�D�N�D��D��RD���D�>�DژRD��D�D�K�D�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��A�VA��A��A��yA��TA��/A��/A��#A��A��A��#A��#A��#A��#A��#A��#A��A��A��#A��#A��#A��A��#A��#A��A���A�ƨAѩ�AхA�p�A�\)A���A��A�A�A�A�1A�t�A�{A�JA��jA��;A��mA��#A�A�A���A��;A� �A�dZA���A��hA��!A��A��9A���A��A��A��`A�jA���A�O�A���A��wA�$�A�jA��9A�
=A��HA��+A���A��A�^5A���A��!A�/A��
A�jA�G�A�/A��A���A�O�A�K�A��FA��A�~�A��A�S�A��;A�I�A���A���A�|�A�;dA�JA��#A��A�%A�^5A��9A�t�A��#A���A��A�K�A�XA���A�jA�+A�{A��
A�I�A�+A��A
=A|^5Ay�wAwAuC�Ap�DAkhsAil�AhffAg�
Af�/Ad�A` �A_VA]+AYt�AV��AUK�AQ��AN��AL�RAJ^5AG
=AD�\AC�FAC�AC"�AA�hA>ĜA=��A=�A<z�A:��A9�A8�9A7O�A5p�A3��A2�+A1��A0��A/�A.ZA-�A,ĜA+XA)S�A'�
A'dZA&r�A$�/A"1'A!33A -A��A~�AƨAx�AbNAoA�mA��AM�A�PA+A��A�/A�A�-A�AoA��A^5AE�A�A�A�A	?}A�DA�AS�A��AA%Ax�A��A$�AA�A&�@�@��^@�
=@�~�@�
=@�E�@�x�@���@�I�@��P@�S�@�=q@��@�~�@��T@��u@�|�@�"�@�x�@�9X@��y@��@��@�  @��@�M�@���@�h@�X@�u@�dZ@��@�@��@�C�@��@�j@�ff@�x�@ܴ9@��
@ڇ+@�&�@�z�@׮@�ȴ@��@�G�@���@ԃ@�ƨ@�"�@�n�@�7L@ϕ�@�-@�7L@̣�@̋D@�A�@�b@�33@��@�V@�A�@��
@�t�@��@ŉ7@��/@��m@�o@���@�-@��D@��;@��@��!@�n�@�hs@� �@�b@�\)@���@��\@�E�@��T@�p�@��u@�9X@���@�o@���@��@�V@��u@��@���@���@���@��h@��@�x�@��@�A�@�dZ@���@��@���@�X@��@�A�@���@�t�@�ȴ@�~�@�^5@�-@��@�/@��@��@�Ĝ@���@�Z@�1@��w@���@�\)@��@���@�M�@��@��@���@���@�`B@�7L@���@��@���@��F@�|�@�dZ@�33@���@�M�@�-@��-@��h@��7@�p�@�/@�Ĝ@�r�@�9X@��m@���@���@�S�@��@�@�ȴ@��\@�ff@�@��T@��-@��h@��@�p�@�X@�X@�G�@��@���@�j@� �@���@���@�dZ@�+@���@��y@��@��R@���@�~�@�^5@�M�@�E�@�-@�@���@��h@�X@�&�@��@�1'@���@��@�@�ȴ@��+@�=q@��@�J@��#@���@��@�?}@��@��D@�Q�@�b@�  @��m@��@��@�dZ@�C�@���@��R@�~�@�-@��T@���@��@�/@��@���@��@��D@�r�@�Q�@�  @���@��@�v�@�E�@�-@��@���@���@�p�@��@���@���@��@�r�@�9X@�1@��;@���@�S�@�+@���@���@�E�@���@���@��@���@���@��@�z�@�Q�@�A�@��w@�l�@�C�@�K�@�;d@�33@���@��y@��!@�v�@�5?@�-@��@��-@��h@��@�p�@��`@�z�@y7L@n5?@e�@_�;@W��@P1'@G�@A%@:�\@4�/@.�R@(r�@#�m@��@~�@��@�7@��@bN111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��A�VA��A��A��yA��TA��/A��/A��#A��A��A��#A��#A��#A��#A��#A��#A��A��A��#A��#A��#A��A��#A��#A��A���A�ƨAѩ�AхA�p�A�\)A���A��A�A�A�A�1A�t�A�{A�JA��jA��;A��mA��#A�A�A���A��;A� �A�dZA���A��hA��!A��A��9A���A��A��A��`A�jA���A�O�A���A��wA�$�A�jA��9A�
=A��HA��+A���A��A�^5A���A��!A�/A��
A�jA�G�A�/A��A���A�O�A�K�A��FA��A�~�A��A�S�A��;A�I�A���A���A�|�A�;dA�JA��#A��A�%A�^5A��9A�t�A��#A���A��A�K�A�XA���A�jA�+A�{A��
A�I�A�+A��A
=A|^5Ay�wAwAuC�Ap�DAkhsAil�AhffAg�
Af�/Ad�A` �A_VA]+AYt�AV��AUK�AQ��AN��AL�RAJ^5AG
=AD�\AC�FAC�AC"�AA�hA>ĜA=��A=�A<z�A:��A9�A8�9A7O�A5p�A3��A2�+A1��A0��A/�A.ZA-�A,ĜA+XA)S�A'�
A'dZA&r�A$�/A"1'A!33A -A��A~�AƨAx�AbNAoA�mA��AM�A�PA+A��A�/A�A�-A�AoA��A^5AE�A�A�A�A	?}A�DA�AS�A��AA%Ax�A��A$�AA�A&�@�@��^@�
=@�~�@�
=@�E�@�x�@���@�I�@��P@�S�@�=q@��@�~�@��T@��u@�|�@�"�@�x�@�9X@��y@��@��@�  @��@�M�@���@�h@�X@�u@�dZ@��@�@��@�C�@��@�j@�ff@�x�@ܴ9@��
@ڇ+@�&�@�z�@׮@�ȴ@��@�G�@���@ԃ@�ƨ@�"�@�n�@�7L@ϕ�@�-@�7L@̣�@̋D@�A�@�b@�33@��@�V@�A�@��
@�t�@��@ŉ7@��/@��m@�o@���@�-@��D@��;@��@��!@�n�@�hs@� �@�b@�\)@���@��\@�E�@��T@�p�@��u@�9X@���@�o@���@��@�V@��u@��@���@���@���@��h@��@�x�@��@�A�@�dZ@���@��@���@�X@��@�A�@���@�t�@�ȴ@�~�@�^5@�-@��@�/@��@��@�Ĝ@���@�Z@�1@��w@���@�\)@��@���@�M�@��@��@���@���@�`B@�7L@���@��@���@��F@�|�@�dZ@�33@���@�M�@�-@��-@��h@��7@�p�@�/@�Ĝ@�r�@�9X@��m@���@���@�S�@��@�@�ȴ@��\@�ff@�@��T@��-@��h@��@�p�@�X@�X@�G�@��@���@�j@� �@���@���@�dZ@�+@���@��y@��@��R@���@�~�@�^5@�M�@�E�@�-@�@���@��h@�X@�&�@��@�1'@���@��@�@�ȴ@��+@�=q@��@�J@��#@���@��@�?}@��@��D@�Q�@�b@�  @��m@��@��@�dZ@�C�@���@��R@�~�@�-@��T@���@��@�/@��@���@��@��D@�r�@�Q�@�  @���@��@�v�@�E�@�-@��@���@���@�p�@��@���@���@��@�r�@�9X@�1@��;@���@�S�@�+@���@���@�E�@���@���@��@���@���@��@�z�@�Q�@�A�@��w@�l�@�C�@�K�@�;d@�33@���@��y@��!@�v�@�5?@�-@��@��-@��h@��@�p�@��`@�z�@y7L@n5?@e�@_�;@W��@P1'@G�@A%@:�\@4�/@.�R@(r�@#�m@��@~�@��@�7@��@bN111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBr�Bq�Bq�Bq�Bp�Bq�Bq�Bq�Bq�Bq�Bq�Bq�Br�Bq�Bq�Br�Br�Bq�Br�Br�Br�Br�Br�Br�Br�Br�Bs�Bt�Bx�B�7B��B��B��B�Bq�Bs�BjB\)BQ�BJ�BD�B=qB<jB<jB<jB<jB<jB>wB<jB<jB:^B<jBN�BVBI�B?}B5?B<jB;dB@�B;dB2-B"�BB�fBɺB�B��B�1Bl�BW
BL�B;dB5?B �B�B�B�B�BoB�B��B�jB�3B��B�7B~�Bt�Bl�BcTB^5B[#BVBR�BM�BH�BA�B7LB)�B\B
�B
�B
�^B
��B
��B
��B
�%B
~�B
z�B
x�B
s�B
k�B
]/B
M�B
C�B
1'B
�B
VB	��B	�/B	��B	�FB	�!B	�B	��B	�uB	�B	z�B	n�B	]/B	N�B	E�B	33B	$�B	�B	bB	B	B	+B		7B	
=B	1B	  B��B��B��B�B�B�`B�BB�B��B��B��BȴBƨB��B�wB�dB�FB�B�B��B��B��B��B��B�{B�oB�bB�JB�+B�B�B~�B}�Bz�B{�Bz�By�Bw�Bt�BiyBdZBcTBe`BdZBdZBgmBiyBiyBffBdZBcTBaHB`BBaHB]/BVBS�BS�BXB\)BXBM�BK�BYBYBbNBdZBcTBcTBcTBdZBcTBe`BhsBiyBiyBk�Bl�Bk�Bl�Bk�Bk�Bl�Bl�Bl�Bl�Bm�Bl�Bl�Bk�Bk�Bk�Bm�Bn�Bo�Bo�Bo�Bp�Br�Bt�Bt�Bv�Bx�By�B{�B}�B� B� B�B�B�B�B�B�B�1B�DB�VB�bB�hB�hB�oB�hB�uB��B��B��B��B��B��B��B��B��B��B��B��B�B�!B�9B�9B�?B�XB��B��BŢBɺBɺB��B��B��B��B��B�B�5B�BB�`B�mB�yB�B�B�B��B��B��B	B	%B	DB	hB	�B	�B	�B	�B	$�B	'�B	(�B	,B	0!B	33B	49B	5?B	:^B	?}B	@�B	B�B	E�B	F�B	H�B	J�B	M�B	N�B	O�B	Q�B	T�B	YB	[#B	\)B	^5B	_;B	cTB	e`B	gmB	gmB	iyB	jB	l�B	m�B	n�B	p�B	s�B	t�B	v�B	w�B	w�B	w�B	x�B	z�B	|�B	}�B	�B	�B	�B	�+B	�7B	�7B	�=B	�JB	�VB	�hB	�oB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�-B	�3B	�9B	�LB	�LB	�^B	�dB	�}B	��B	��B	ÖB	ĜB	ŢB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�B	�#B	�)B	�/B	�5B	�;B	�NB	�TB	�TB	�ZB	�ZB	�`B	�`B	�fB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
%B
1B
	7B
	7B
	7B
bB
�B
�B
%�B
-B
2-B
7LB
>wB
E�B
I�B
N�B
S�B
YB
_;B
cTB
ffB
k�B
o�B
u�B
y�B
~�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  Br�Bq�Bq�Bq�Bp�Bq�Bq�Bq�Bq�Bq�Bq�Bq�Br�Bq�Bq�Br�Br�Bq�Br�Br�Br�Br�Br�Br�Br�Br�Bs�Bt�Bx�B�7B��B��B��B�Bq�Bs�Bj�B\+BQ�BJ�BD�B=xB<qB<tB<qB<pB<sB>�B<sB<vB:`B<rBN�BVBI�B?�B5IB<rB;kB@�B;gB22B"�BB�jBɻB�B��B�5Bl�BWBL�B;iB5@B �B�B�B�B�BrB�B��B�oB�7B��B�?B~�Bt�Bl�BcXB^;B[)BVBR�BM�BH�BA�B7QB*B`B
�B
�B
�eB
��B
��B
��B
�+B
B
z�B
x�B
s�B
k�B
]6B
M�B
C�B
12B
�B
`B	��B	�<B	��B	�UB	�/B	�B	��B	��B	�B	z�B	n�B	]@B	N�B	E�B	3GB	$�B	�B	yB	4B	/B	AB		LB	
QB	FB	 B�B��B��B�B�B�xB�YB�'B�B��B��B��BƾB��B��B�|B�^B�4B�B�B��B��B��B��B��B��B�}B�dB�EB�4B�"BB~Bz�B|Bz�By�Bw�Bt�Bi�BduBcoBeyBdtBduBg�Bi�Bi�Bf�BduBcoBaeB`_BadB]HBV BTBTBX.B\DBX/BM�BK�BY1BY4BbjBdtBcoBcqBcnBduBcoBe|Bh�Bi�Bi�Bk�Bl�Bk�Bl�Bk�Bk�Bl�Bl�Bl�Bl�Bm�Bl�Bl�Bk�Bk�Bk�Bm�Bn�Bo�Bo�Bo�Bp�Br�Bt�Bt�Bv�Bx�By�B|B~B�B�B�&B�'B�&B�.B�4B�:B�KB�]B�oB�}B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B� B�B�*B�8B�OB�RB�XB�nB��B��BŻB��B��B��B��B��B�B�B�%B�KB�XB�wB�B�B��B�B��B��B��B��B	B	8B	XB	|B	�B	�B	�B	�B	$�B	(B	)B	,B	05B	3DB	4JB	5RB	:oB	?�B	@�B	B�B	E�B	F�B	H�B	J�B	M�B	N�B	O�B	Q�B	UB	Y'B	[2B	\9B	^EB	_LB	cdB	eqB	g|B	g~B	i�B	j�B	l�B	m�B	n�B	p�B	s�B	t�B	v�B	w�B	w�B	w�B	x�B	z�B	|�B	~B	�B	�B	�#B	�8B	�DB	�GB	�MB	�ZB	�fB	�wB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�"B	�.B	�9B	�AB	�HB	�ZB	�ZB	�lB	�rB	��B	��B	��B	äB	īB	ůB	��B	��B	��B	��B	��B	��B	��B	��B	� B	��B	�	B	�B	�B	�B	�B	�*B	�1B	�7B	�>B	�DB	�HB	�YB	�`B	�`B	�hB	�fB	�jB	�kB	�rB	�{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 B
B
B
B
B
B
B
B
%B
&B
)B
-B
0B
<B
	BB
	AB
	BB
mB
�B
�B
%�B
-B
27B
7UB
>�B
E�B
I�B
N�B
T B
Y!B
_CB
c]B
fsB
k�B
o�B
u�B
y�B
111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.04 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311214262016053112142620160531121426  AO  ARCAADJP                                                                    20140721230512    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721230512  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721230512  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531121426  IP                  G�O�G�O�G�O�                