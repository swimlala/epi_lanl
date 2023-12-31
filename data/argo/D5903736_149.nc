CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-11-21T04:11:50Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         ZPRIMARY | https://orcid.org/0000-0001-7324-3159 | Matthew Alkire, University of Washington        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7(   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7,   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    70   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7@   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7P   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7`   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7h   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    8   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    8    DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     8$   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8D   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8H   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8L   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8l   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8�   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8�   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           8�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
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
_FillValue                    ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �0   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �@   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �D   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �T   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �X   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �\   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �`Argo profile    3.1 1.2 19500101000000  20181121041150  20211228103551  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  4051                            2C  D   APEX                            5368                            041511                          846 @ׯOJ(1   @ׯ�+�&@2�$�/�d6V�u1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR�CT�CV  CX  CZ�C\�C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!y�D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dty�DyUD�\D�8�D�w\D��HD�=D�NfD�o�D���D� D�3�D��fD�ҏD��
D�:=DځHD��)D� RD�D{D�\D��R11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��R@��RA\)A?\)A_\)A\)A��A��A��A��AϮA߮A�A��B�
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
B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CR]CT]CU��CW��CZ]C\]C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D }qD �qD}qD�qD}qD�qD}qD�D}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD	}qD	�qD
}qD
�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD }qD �qD!wD!�qD"}qD"�qD#}qD#�qD$}qD$�qD%}qD%�qD&}qD&�qD'}qD'�qD(}qD(�qD)}qD)�qD*}qD*�qD+}qD+�qD,}qD,�qD-}qD-�qD.}qD.�qD/}qD/�qD0}qD0�qD1}qD1�qD2}qD2�qD3}qD3�qD4}qD4�qD5}qD5�qD6}qD6�qD7}qD7�qD8}qD8�qD9}qD9�qD:}qD:�qD;}qD;�qD<}qD<�qD=}qD=�qD>}qD>�qD?}qD?�qD@}qD@�qDA}qDA�qDB}qDB�qDC}qDC�qDD}qDD�qDE}qDE�qDF}qDF�qDG}qDG�qDH}qDH�qDI}qDI�qDJ}qDJ�qDK}qDK�qDL}qDL�qDM}qDM�qDN}qDN�qDO}qDO�qDP}qDP�qDQ}qDQ�qDR}qDR�qDS}qDS�qDT}qDT�qDU}qDU�qDV}qDV�qDW}qDW�qDX}qDX�qDY}qDY�qDZ}qDZ�qD[}qD[�qD\}qD\�qD]}qD]�qD^}qD^�qD_}qD_�qD`}qD`�qDa}qDa�qDb}qDb�qDc}qDc�qDd}qDd�qDe}qDe�qDf}qDf�qDg}qDg�qDh}qDh�qDi}qDi�qDj}qDj�qDk}qDk�qDl}qDl�qDm}qDm�qDn}qDn�qDo}qDo�qDp}qDp�qDq}qDq�qDr}qDr�qDs}qDs�qDtwDyR�D�D�7�D�vD�� D��D�MD�nfD�ǮD��D�2=D��D��GD���D�8�Dڀ D���D��
D�C3D�D��
11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AӬA���A�O�A���A���AѼjAѝ�A�n�A�C�A�oA��mA��
A���A���AиRAЮAЍPA�hsA�I�A�=qA�1'A�
=A��A��;AϮAΕ�AͲ-A��A�z�A���A�bA�^5Aǥ�A�JA�"�A��#A�A�=qA�ƨA���A��^A��A���A�dZA�/A�A���A�Q�A�1A�I�A�|�A���A�;dA�p�A���A��9A���A��A�|�A��PA�S�A��wA��hA���A�r�A�A�A��jA���A�\)A��A��wA�\)A��TA��DA�VA�-A�XA�x�A�&�A��A���A�A�  A�?}A�ȴA�^5A�|�A���A���A��A�\)A��A�(�A��TA��mA�r�A��A�&�A��DA�7LA�bA�S�A�-A�oA��/A��A�33A��A�VA�jA���A���A�~�A�r�A��A��mA�9XA�oA�A}`BA|��A|^5A|1'A{G�Ay`BAw��Av1At�+Ar�HAq�Ao
=An(�Ak�#AiAg��Ae;dAb�DA_�hA\��AY��AV�HAV9XAT~�ARȴAPn�AO/AK�wAJA�AI/AH  AF��AEƨADv�AC��AC
=AAt�A@bA=��A<��A<1A;XA:  A9K�A8��A7�TA6bNA5\)A3`BA25?A0VA/|�A.�/A.VA,��A+\)A*��A)�PA(�HA'��A&1'A$E�A#7LA"��A!�hA �A (�A~�A��A$�Ap�An�A�#A�mA�7Av�A��A��A\)A�A+A��AVA��AoA
$�A	%AffAVA�A��A�AA1'A�FA%A r�A @��@���@��@�  @�o@��7@���@�~�@�X@�@�V@�
=@�u@���@�E�@�^@�p�@�O�@�?}@�C�@䛦@���@�@�x�@�ȴ@�@�j@��y@պ^@�hs@�O�@���@�5?@�j@ϝ�@��H@���@�I�@˝�@�S�@�n�@�J@���@ǍP@ƸR@Ɨ�@Ɨ�@Ə\@�=q@ź^@�9X@���@�@�-@���@�l�@��@��h@���@��w@�l�@�+@�@���@�=q@��@��^@��h@�r�@�ƨ@�C�@�+@��@�
=@��@��!@�^5@��#@�X@�%@�9X@�+@���@��T@���@�`B@�/@�V@���@�1@��
@��
@���@��F@���@�"�@��!@�@��@��;@��@��!@���@�b@���@�"�@��!@�E�@��^@�hs@�?}@�V@���@���@�z�@�I�@��m@�S�@���@�M�@�x�@�7L@�G�@�X@�X@�O�@�V@��`@��@���@�I�@���@��@�"�@�~�@�ff@�v�@�^5@�v�@�@��^@���@�x�@�p�@�G�@�Ĝ@��
@�l�@��H@�J@��T@���@�X@��T@�`B@���@�Ĝ@���@�E�@�5?@���@��`@�Z@�Q�@�I�@�K�@��y@�@���@��\@�~�@�=q@���@��T@�M�@�E�@��T@���@�hs@��@�Ĝ@�z�@�9X@�A�@��@���@�ƨ@�dZ@�|�@�
=@�ȴ@���@��@���@��R@���@���@�"�@��R@�ff@�=q@�{@��@���@��@��@�z�@�Z@��@�  @��@��w@�|�@�l�@�\)@��@���@�n�@���@��H@��R@�@��7@�hs@�hs@���@���@���@��@��F@��P@�K�@�o@���@���@���@��\@�n�@�=q@�J@�J@�J@���@���@�@��#@��@�7L@��@���@��@���@��D@� �@�|�@�K�@�S�@�t�@���@�+@��R@�n�@���@�@�p�@�&�@���@��j@�bN@�t�@�o@�+k@u��@m��@e�o@_��@SA�@L[�@Ec@=��@5-w@/C@&��@"{�@R�@K�@�@A�@�@
�c@Q@ƨ11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   AӬA���A�O�A���A���AѼjAѝ�A�n�A�C�A�oA��mA��
A���A���AиRAЮAЍPA�hsA�I�A�=qA�1'A�
=A��A��;AϮAΕ�AͲ-A��A�z�A���A�bA�^5Aǥ�A�JA�"�A��#A�A�=qA�ƨA���A��^A��A���A�dZA�/A�A���A�Q�A�1A�I�A�|�A���A�;dA�p�A���A��9A���A��A�|�A��PA�S�A��wA��hA���A�r�A�A�A��jA���A�\)A��A��wA�\)A��TA��DA�VA�-A�XA�x�A�&�A��A���A�A�  A�?}A�ȴA�^5A�|�A���A���A��A�\)A��A�(�A��TA��mA�r�A��A�&�A��DA�7LA�bA�S�A�-A�oA��/A��A�33A��A�VA�jA���A���A�~�A�r�A��A��mA�9XA�oA�A}`BA|��A|^5A|1'A{G�Ay`BAw��Av1At�+Ar�HAq�Ao
=An(�Ak�#AiAg��Ae;dAb�DA_�hA\��AY��AV�HAV9XAT~�ARȴAPn�AO/AK�wAJA�AI/AH  AF��AEƨADv�AC��AC
=AAt�A@bA=��A<��A<1A;XA:  A9K�A8��A7�TA6bNA5\)A3`BA25?A0VA/|�A.�/A.VA,��A+\)A*��A)�PA(�HA'��A&1'A$E�A#7LA"��A!�hA �A (�A~�A��A$�Ap�An�A�#A�mA�7Av�A��A��A\)A�A+A��AVA��AoA
$�A	%AffAVA�A��A�AA1'A�FA%A r�A @��@���@��@�  @�o@��7@���@�~�@�X@�@�V@�
=@�u@���@�E�@�^@�p�@�O�@�?}@�C�@䛦@���@�@�x�@�ȴ@�@�j@��y@պ^@�hs@�O�@���@�5?@�j@ϝ�@��H@���@�I�@˝�@�S�@�n�@�J@���@ǍP@ƸR@Ɨ�@Ɨ�@Ə\@�=q@ź^@�9X@���@�@�-@���@�l�@��@��h@���@��w@�l�@�+@�@���@�=q@��@��^@��h@�r�@�ƨ@�C�@�+@��@�
=@��@��!@�^5@��#@�X@�%@�9X@�+@���@��T@���@�`B@�/@�V@���@�1@��
@��
@���@��F@���@�"�@��!@�@��@��;@��@��!@���@�b@���@�"�@��!@�E�@��^@�hs@�?}@�V@���@���@�z�@�I�@��m@�S�@���@�M�@�x�@�7L@�G�@�X@�X@�O�@�V@��`@��@���@�I�@���@��@�"�@�~�@�ff@�v�@�^5@�v�@�@��^@���@�x�@�p�@�G�@�Ĝ@��
@�l�@��H@�J@��T@���@�X@��T@�`B@���@�Ĝ@���@�E�@�5?@���@��`@�Z@�Q�@�I�@�K�@��y@�@���@��\@�~�@�=q@���@��T@�M�@�E�@��T@���@�hs@��@�Ĝ@�z�@�9X@�A�@��@���@�ƨ@�dZ@�|�@�
=@�ȴ@���@��@���@��R@���@���@�"�@��R@�ff@�=q@�{@��@���@��@��@�z�@�Z@��@�  @��@��w@�|�@�l�@�\)@��@���@�n�@���@��H@��R@�@��7@�hs@�hs@���@���@���@��@��F@��P@�K�@�o@���@���@���@��\@�n�@�=q@�J@�J@�J@���@���@�@��#@��@�7L@��@���@��@���@��D@� �@�|�@�K�@�S�@�t�@���@�+@��R@�n�@���@�@�p�@�&�@���@��j@�bN@�t�G�O�@�+k@u��@m��@e�o@_��@SA�@L[�@Ec@=��@5-w@/C@&��@"{�@R�@K�@�@A�@�@
�c@Q@ƨ11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBPBJBJBJBJBDBDBDBJBJBPBVBVBVBVB\B\BbBbBbBbBVBPBDB1B��B��B��B��BBPB�B!�BB�BT�BZBl�B�B�%B�=B��B�!B�?B�LB�RB�}B�}B��BÖB�wB�jBŢB��B��B�#B�)B�
B��B��BȴB�jB�RB�B��B�PBw�Bn�Be`B_;B\)BYBYBXBaHBe`B`BBK�BC�BA�B=qBI�BJ�B<jB33B,B�B	7B��B�BɺB��B�B��B�=Bv�BhsBXBI�BD�B1'B&�B�B�B#�B#�B �B�B�BPB
��B
�fB
��B
��B
��B
ǮB
�?B
��B
��B
�PB
z�B
p�B
l�B
hsB
[#B
H�B
=qB
@�B
6FB
(�B
�B
%B	��B	�B	�#B	��B	�jB	��B	�uB	�B	t�B	bNB	[#B	N�B	E�B	;dB	2-B	$�B	�B	�B	hB	VB		7B	B��B��B��B�B�mB�ZB�ZB�HB�5B�#B�B��B��B��BB�qB�RB�?B�RB�^B�LB�3B�'B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�bB�DB�+B�B�B�B|�Bx�Bx�Bt�Bu�Bu�Bu�Bt�Bs�Bq�Bq�Bq�Br�Bt�Bt�Bs�Bs�Bs�Bs�Bs�Bs�Bs�Br�Br�Bv�Bw�Bx�B{�B� B|�B~�B�B�B�B�B�B�B�1B�=B�DB�=B�VB�oB�uB��B��B��B��B��B��B��B��B��B��B�B�B�!B�!B�-B�3B�FB�^B�qBÖBƨBƨBȴBǮBȴB��B��B��B��B�
B�/B�5B�BB�fB�yB�B�B�B�B��B��B��B	  B	B	B		7B	
=B	DB	PB	PB	VB	VB	\B	\B	PB	hB	uB	�B	�B	�B	�B	�B	�B	�B	 �B	 �B	 �B	 �B	!�B	$�B	%�B	(�B	+B	,B	/B	33B	0!B	8RB	<jB	>wB	@�B	A�B	D�B	G�B	H�B	J�B	L�B	N�B	P�B	Q�B	S�B	W
B	XB	ZB	_;B	cTB	ffB	iyB	jB	l�B	k�B	k�B	o�B	o�B	o�B	p�B	v�B	u�B	u�B	u�B	y�B	{�B	~�B	�B	�B	�B	�+B	�=B	�JB	�PB	�PB	�PB	�bB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�'B	�'B	�!B	�B	�'B	�3B	�-B	�9B	�LB	�LB	�RB	�XB	�RB	�^B	�jB	��B	��B	B	B	ÖB	ĜB	ŢB	ŢB	ŢB	ŢB	ƨB	ƨB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�
B	�B	�B	�#B	�#B	�)B	�/B	�5B	�;B	�;B	�BB	�HB	�HB	�HB	�TB	�TB	�ZB	�fB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
+B
%B
%B
%B
%B
+B
+B
+B
+B
%B
%B
%B
	lB
�B
"NB
,�B
0�B
:^B
C�B
K^B
SuB
Z�B
`�B
f�B
jB
n�B
q'B
u�B
zB
}<B
��B
��B
�l11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B�B�B�B�B�B
�B
�B
�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B
�B�B�iB�CB�HB�vB�B�B
B!HBBBTwBY�BlB��B��B��B�<B��B��B��B��B��B��B�B�B��B��B�B�ZB�{BڥBۡBֆB�oB�XB�4B��B��B��B�B��BwLBnBd�B^�B[�BX�BX�BW�B`�Bd�B_�BK@BCBAB<�BI9BJ:B;�B2�B+�BB�B�HB׋B�5B�B��B�5B��BvGBg�BW�BI7BDB0�B&fB-B8B#TB#VB EB&BB�B
�uB
��B
�RB
�RB
�KB
�-B
��B
�RB
� B
��B
z`B
p#B
lB
g�B
Z�B
H3B
<�B
@B
5�B
(tB
B
�B	�oB	��B	ڤB	�EB	��B	�bB	��B	��B	t=B	a�B	Z�B	NWB	E B	:�B	1�B	$bB	8B	B	�B	�B	�B	�B�zB�fB�GB�B��B��B��B��BݶBڨB׏BԅB�gB�OB�B��B��B��B��B��B��B��B��B��B��B�hB�MB�0B�(B�-B�&B�)B�<B�FB�0B� B�B�B�
B�B��B��B��B��B��B��B|sBxYBxVBt=BuGBuDBuABt>Bs9Bq/Bq,Bq*Br3Bt=Bt?Bs:Bs7Bs8Bs8Bs9Bs;Bs:Br3Br3BvMBwQBxXB{mB�B|sB~}B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�1B�7B�/B�,B�UB�sB�rB�B��B��B��B��B��B��B��B��B��B�B�+B�(B�2B�1B�8B�BB�PB�ZB�rB֍BܱBݸB��B��B��B�B�B�B�(B�YB�]B�dB��B	�B	�B	�B		�B	
�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	B	B	B	B	#B	&B	AB	 HB	 IB	 IB	 KB	!NB	$^B	%eB	(yB	*�B	+�B	.�B	2�B	/�B	7�B	;�B	=�B	@B	AB	D B	G3B	H4B	JDB	LOB	N\B	PjB	QmB	S{B	V�B	W�B	Y�B	^�B	b�B	e�B	h�B	j B	lB	kB	kB	o#B	o!B	o!B	p)B	vMB	uEB	uFB	uFB	y_B	{kB	~~B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�,B	�OB	�TB	�WB	�ZB	�|B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�
B	�B	�B	�B	�B	�$B	�$B	�%B	�(B	�&B	�+B	�3B	�;B	�OB	�QB	�QB	�OB	�VB	�cB	�kB	�rB	�uB	ՇB	ՆB	ՇB	ՅB	֍B	؜B	ؘB	ڦB	ڧB	۪B	ܯB	ݵB	޽B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	��B	��B	�B	�B	�B	�(B	�*B	�,B	�6B	�2B	�4B	�KB	�OB	�YB	�\B	�cB	�kB	�wB	�zB	�xB	�|B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�G�O�B
�B
B
!�B
,wB
0tB
9�B
CMB
J�B
R�B
ZpB
`B
f5B
jB
nQB
p�B
uB
y�B
|�B
�@B
�B
��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.04 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =1(+/-0), vertically averaged dS =-0.001(+/-0.002) in PSS-78.                                                                                                                                                                                                 Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202112281035512021122810355120211228103551  AO  ARCAADJP                                                                    20181121041150    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181121041150  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181121041150  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20211228103551  IP                  G�O�G�O�G�O�                