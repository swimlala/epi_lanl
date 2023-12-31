CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-05-03T02:15:28Z AOML 3.0 creation; 2016-05-31T19:14:43Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20150503021528  20160531121443  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               oA   AO  4051_7090_111                   2C  D   APEX                            5368                            041511                          846 @�M��j?�1   @�M��ۿ�@4BI�^5�dNE����1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    oA   A   A   @�  @���@���A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bi��Bm��Bw��B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C�C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0�C2�C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW�fDX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  DtffDy��D� D�I�D��3D��3D�	�D�<�D�� D��3D��3D�FfD�vfDǹ�D�fD�FfDډ�D��3D�fD�9�D�D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @}p�@��@��A\)A?\)A_\)A\)A��A��A��A��AϮA߮A�A��B�
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
Bip�Bmp�Bwp�B�
B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C]C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C0]C2]C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D }qD �qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD	}qD	�qD
}qD
�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD }qD �qD!}qD!�qD"}qD"�qD#}qD#�qD$}qD$�qD%}qD%�qD&}qD&�qD'}qD'�qD(}qD(�qD)}qD)�qD*}qD*�qD+}qD+�qD,}qD,�qD-}qD-�qD.}qD.�qD/}qD/�qD0}qD0�qD1}qD1�qD2}qD2�qD3}qD3�qD4}qD4�qD5}qD5�qD6}qD6�qD7}qD7�qD8}qD8�qD9}qD9�qD:}qD:�qD;}qD;�qD<}qD<�qD=}qD=�qD>}qD>�qD?}qD?�qD@}qD@�qDA}qDA�qDB}qDB�qDC}qDC�qDD}qDD�qDE}qDE�qDF}qDF�qDG}qDG�qDH}qDH�qDI}qDI�qDJ}qDJ�qDK}qDK�qDL}qDL�qDM}qDM�qDN}qDN�qDO}qDO�qDP}qDP�qDQ}qDQ�qDR}qDR�qDS}qDS�qDT}qDT�qDU}qDU�qDV}qDV�qDW��DW�qDX}qDX�qDY}qDY�qDZ}qDZ�qD[}qD[�qD\}qD\�qD]}qD]�qD^}qD^�qD_}qD_�qD`}qD`�qDa}qDa�qDb}qDb�qDc}qDc�qDd}qDd�qDe}qDe�qDf}qDf�qDg}qDg�qDh}qDh�qDi}qDi�qDj}qDj�qDk}qDk�qDl}qDl�qDm}qDm�qDn}qDn�qDo}qDo�qDp}qDp�qDq}qDq�qDr}qDr�qDs}qDs�qDtc�Dy�>D��D�HRD���D���D�RD�;�D���D���D���D�ED�uDǸRD�D�EDڈRD���D�D�8RD�RD���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AѸRAѸRAѣ�AыDA�~�A�z�A�x�A�r�A�n�A�p�A�l�A�ffA�jA�jA�jA�jA�jA�jA�jA�l�A�ffA�dZA�bNA�bNA�dZA�dZA�dZA�XA��A�|�A�~�A���A�~�A�t�A���A�x�A�I�A�1AŲ-A�/A�hsAìA�l�A��A�ZA�33A��A���A��mA��#A��jA��uA�|�A�VA���A�M�A���A��jA��PA�l�A�7LA�JA��mA��A�ZA���A�Q�A�dZA�K�A��;A�p�A�oA��-A��!A���A��/A���A�G�A�ȴA�bNA��A��A�ȴA���A���A��^A�x�A�  A�=qA�=qA�VA��uA�r�A�
=A��mA��\A�A�oA�  A�C�A�z�A�~�A��TA�5?A�oA�-A�{A�z�A�\)A���A�XA�~�A�A�5?A��/A��A�bA�x�A�wA~I�A{�AwK�At�AsS�Aqx�An��Al�AiO�AioAfJAc`BAb �A`�A^��A^jA]�A[�7AZ=qAW"�AV�AUƨAU`BAR{AMƨAKx�AGXAE�PAE�ADȴAD{A?��A<�HA<r�A:�A9l�A7�A6�`A6  A5|�A4n�A3��A3A1��A.�DA,�\A,(�A+��A*�A*E�A(^5A& �A$�RA$-A#��A"��A"ffAXA��A/Ap�A��A��A+A$�A��A��A��A��A�AffA�wA�A�9A|�A5?A�yAbNA�A
��A	�A	�A	VA-AdZA"�AȴA�AXA�!A33A=qA 1'@�G�@��@�33@���@�l�@���@��@�S�@�~�@���@�x�@�A�@��@�r�@��@��@��m@�@�&�@��`@�9@��@�u@�b@���@�9@��@���@��T@� �@�ff@���@�V@���@���@��@��#@ՙ�@�G�@�Ĝ@�I�@��@�/@·+@��@�33@��@Ǯ@�K�@��@�{@�?}@ċD@�(�@��m@�S�@�o@§�@��h@��D@�b@�K�@��R@���@�M�@�G�@��j@� �@��
@�+@���@��\@�ff@���@�x�@�hs@�X@��@� �@��
@�ƨ@���@�|�@�\)@��R@���@�`B@�/@���@��9@��@�9X@�  @���@��@��F@�|�@�o@�ȴ@�{@��-@�p�@�%@���@�%@�%@�z�@�1'@��@�\)@�33@�o@�
=@���@�5?@��#@���@��@�hs@�X@��`@�1'@��y@��R@��!@���@���@�E�@��@���@�hs@��`@��j@�bN@���@�dZ@�S�@�C�@�C�@�"�@��H@���@�E�@���@�/@���@�Ĝ@���@�A�@� �@�b@��;@��F@��P@�|�@�dZ@�C�@�@�~�@�E�@��@���@��@���@��7@�7L@��@��@���@���@�hs@�hs@��j@�1'@��w@�33@�;d@�C�@�S�@�;d@�"�@�"�@�;d@�l�@��@��w@��w@���@��m@��;@��;@���@��
@� �@�  @���@���@���@�n�@�@�G�@��u@�I�@�(�@�b@�ƨ@�@��@�b@��
@���@���@��m@��@���@�1@�b@�1@�  @�b@�(�@�1'@� �@�1@��
@�dZ@�;d@���@�~�@�J@��#@�O�@��D@�Z@�1@��
@���@�dZ@�K�@�K�@�"�@���@�5?@��@���@�/@���@��@��@�b@�l�@���@�E�@���@��@���@�Q�@��@�dZ@�;d@�@��R@�v�@�5?@��@�@��-@���@�`B@��/@�z�@�1'@���@��w@���@�dZ@��h@� �@up�@i%@bM�@Y��@O�P@IG�@C"�@>��@8��@4�D@.�R@(�@#"�@K�@�!@\)@t�@  @�/11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   AѸRAѸRAѣ�AыDA�~�A�z�A�x�A�r�A�n�A�p�A�l�A�ffA�jA�jA�jA�jA�jA�jA�jA�l�A�ffA�dZA�bNA�bNA�dZA�dZA�dZA�XA��A�|�A�~�A���A�~�A�t�A���A�x�A�I�A�1AŲ-A�/A�hsAìA�l�A��A�ZA�33A��A���A��mA��#A��jA��uA�|�A�VA���A�M�A���A��jA��PA�l�A�7LA�JA��mA��A�ZA���A�Q�A�dZA�K�A��;A�p�A�oA��-A��!A���A��/A���A�G�A�ȴA�bNA��A��A�ȴA���A���A��^A�x�A�  A�=qA�=qA�VA��uA�r�A�
=A��mA��\A�A�oA�  A�C�A�z�A�~�A��TA�5?A�oA�-A�{A�z�A�\)A���A�XA�~�A�A�5?A��/A��A�bA�x�A�wA~I�A{�AwK�At�AsS�Aqx�An��Al�AiO�AioAfJAc`BAb �A`�A^��A^jA]�A[�7AZ=qAW"�AV�AUƨAU`BAR{AMƨAKx�AGXAE�PAE�ADȴAD{A?��A<�HA<r�A:�A9l�A7�A6�`A6  A5|�A4n�A3��A3A1��A.�DA,�\A,(�A+��A*�A*E�A(^5A& �A$�RA$-A#��A"��A"ffAXA��A/Ap�A��A��A+A$�A��A��A��A��A�AffA�wA�A�9A|�A5?A�yAbNA�A
��A	�A	�A	VA-AdZA"�AȴA�AXA�!A33A=qA 1'@�G�@��@�33@���@�l�@���@��@�S�@�~�@���@�x�@�A�@��@�r�@��@��@��m@�@�&�@��`@�9@��@�u@�b@���@�9@��@���@��T@� �@�ff@���@�V@���@���@��@��#@ՙ�@�G�@�Ĝ@�I�@��@�/@·+@��@�33@��@Ǯ@�K�@��@�{@�?}@ċD@�(�@��m@�S�@�o@§�@��h@��D@�b@�K�@��R@���@�M�@�G�@��j@� �@��
@�+@���@��\@�ff@���@�x�@�hs@�X@��@� �@��
@�ƨ@���@�|�@�\)@��R@���@�`B@�/@���@��9@��@�9X@�  @���@��@��F@�|�@�o@�ȴ@�{@��-@�p�@�%@���@�%@�%@�z�@�1'@��@�\)@�33@�o@�
=@���@�5?@��#@���@��@�hs@�X@��`@�1'@��y@��R@��!@���@���@�E�@��@���@�hs@��`@��j@�bN@���@�dZ@�S�@�C�@�C�@�"�@��H@���@�E�@���@�/@���@�Ĝ@���@�A�@� �@�b@��;@��F@��P@�|�@�dZ@�C�@�@�~�@�E�@��@���@��@���@��7@�7L@��@��@���@���@�hs@�hs@��j@�1'@��w@�33@�;d@�C�@�S�@�;d@�"�@�"�@�;d@�l�@��@��w@��w@���@��m@��;@��;@���@��
@� �@�  @���@���@���@�n�@�@�G�@��u@�I�@�(�@�b@�ƨ@�@��@�b@��
@���@���@��m@��@���@�1@�b@�1@�  @�b@�(�@�1'@� �@�1@��
@�dZ@�;d@���@�~�@�J@��#@�O�@��D@�Z@�1@��
@���@�dZ@�K�@�K�@�"�@���@�5?@��@���@�/@���@��@��@�b@�l�@���@�E�@���@��@���@�Q�@��@�dZ@�;d@�@��R@�v�@�5?@��@�@��-@���@�`B@��/@�z�@�1'@���@��w@���@�dZ@��h@� �@up�@i%@bM�@Y��@O�P@IG�@C"�@>��@8��@4�D@.�R@(�@#"�@K�@�!@\)@t�@  @�/11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
PB
PB
PB
PB
PB
PB
PB
PB
PB
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
VB
\B
\B
\B
\B
\B
uB
+BBp�B��BÖB��B�B-B6FB7LB8RBA�BM�BXB`BBz�B�9B�HB�B%BPBbB�B$�B&�B33B;dBC�BI�BL�BO�BR�BW
BW
BW
BW
BXB\)BbNB�+B�B�B�B�B�B�bBr�BE�B{B  B�;BȴB��B�B�B��BB%B+B1B��B�NBɺB�XB��B�BG�B.B �B�BuB
=B�B��B�RB��B�PBe`B@�B.B�BbB
��B
�;B
��B
ǮB
�9B
��B
y�B
W
B
M�B
A�B
.B
bB	��B	�B	�`B	��B	ĜB	�?B	�!B	��B	�VB	�%B	z�B	t�B	p�B	ffB	\)B	P�B	@�B	;dB	8RB	33B	!�B	\B	B	B��B��B��B�B�yB�TB�BB�#B�B��B��B��BǮBÖB�}B�dB�FB�B��B��B��B��B��B��B��B�hB�\B�JB�+B�Bx�Bs�Bs�Br�Br�Bu�B{�B~�B}�B|�B}�B~�B� B~�B}�B|�Bz�By�Bx�Bx�Bw�Bv�Bv�Bv�Bv�Bu�Bu�Bv�Bu�Bt�Br�Bq�Bo�Bn�Bm�BjBgmBiyBiyBffBbNB`BBe`BhsBiyBk�BjBjBl�Bo�Bo�Bm�Bk�Bq�Bt�Bw�Bz�B|�B|�B~�B�B�B�B�DB�PB�VB�{B�{B�oB��B��B��B��B��B��B��B��B��B��B��B��B��B�B�FB�FB�LB�jB��BBÖBĜBǮBȴB��B��B�
B�B�#B�)B�)B�;B�NB�fB�B�B��B��B��B��B��B��B	  B	  B	B	
=B	JB	JB	PB	PB	VB	oB	�B	�B	�B	�B	!�B	$�B	(�B	+B	.B	0!B	0!B	0!B	1'B	33B	6FB	8RB	8RB	<jB	?}B	@�B	@�B	E�B	F�B	G�B	J�B	N�B	O�B	P�B	Q�B	T�B	W
B	XB	YB	ZB	ZB	[#B	]/B	bNB	cTB	dZB	dZB	dZB	dZB	e`B	gmB	jB	k�B	k�B	l�B	p�B	r�B	s�B	s�B	t�B	u�B	u�B	v�B	x�B	{�B	}�B	~�B	� B	� B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�%B	�1B	�=B	�JB	�PB	�bB	�uB	�uB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�3B	�LB	�RB	�XB	�jB	�wB	��B	��B	��B	ŢB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ɺB	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�#B	�;B	�NB	�ZB	�fB	�mB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
%B
%B
%B
%B
%B
1B
	7B
DB
DB
JB
JB
\B
hB
�B
�B
%�B
+B
0!B
8RB
=qB
A�B
G�B
L�B
R�B
ZB
_;B
dZB
hsB
jB
n�B
r�B
u�B
y�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B
ZB
ZB
YB
YB
\B
\B
ZB
ZB
ZB
ZB
YB
\B
_B
aB
cB
cB
aB
aB
`B
`B
cB
_B
iB
iB
iB
iB
iB
�B
+BBp�B��BÙB��B�B-B6MB7OB8WBA�BM�BXB`GBz�B�BB�LB�B+BUBfB�B$�B&�B39B;jBC�BI�BL�BO�BR�BWBWBWBWBXB\1BbVB�6B�B�B�B�B�B�jBr�BE�B�B B�=BȺB��B�B�B��BB,B1B7B� B�SB��B�_B��B� BG�B.B �B�BwB
?B�B��B�TB��B�QBecB@�B.B�BgB
� B
�CB
��B
ǴB
�BB
��B
y�B
WB
M�B
A�B
.!B
oB	�B	�B	�lB	�B	ĪB	�MB	�0B	��B	�dB	�4B	z�B	t�B	p�B	fwB	\;B	P�B	@�B	;uB	8dB	3GB	!�B	pB	3B	B�B��B��B�B�B�lB�WB�7B�B�B��B��B��BðB��B�}B�]B� B�B��B��B��B��B��B��B��B�vB�eB�EB�.Bx�Bs�Bs�Br�Br�Bu�B|BB~B}
B~BB�BB~
B}Bz�By�Bx�Bx�Bw�Bv�Bv�Bv�Bv�Bu�Bu�Bv�Bu�Bt�Br�Bq�Bo�Bn�Bm�Bj�Bg�Bi�Bi�Bf�BbjB`_Be}Bh�Bi�Bk�Bj�Bj�Bl�Bo�Bo�Bm�Bk�Bq�Bt�Bw�Bz�B}B}BB�-B�,B�7B�`B�hB�nB��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�3B�\B�[B�bB��B��B¦BìBĵB��B��B��B��B� B�-B�7B�?B�@B�NB�cB�|B��B�B��B��B��B��B�B�B	 B	 B	B	
QB	\B	_B	eB	cB	hB	�B	�B	�B	�B	�B	!�B	$�B	)B	+B	.(B	05B	06B	07B	18B	3EB	6VB	8dB	8cB	<zB	?�B	@�B	@�B	E�B	F�B	G�B	J�B	N�B	O�B	P�B	Q�B	UB	WB	X#B	Y*B	Z.B	Z.B	[5B	]>B	b`B	ceB	dkB	dkB	djB	djB	eqB	g~B	j�B	k�B	k�B	l�B	p�B	r�B	s�B	s�B	t�B	u�B	u�B	v�B	x�B	{�B	~B	B	�B	�B	�B	�B	�B	�$B	�#B	�&B	�'B	�)B	�/B	�4B	�AB	�MB	�ZB	�_B	�sB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�#B	�AB	�ZB	�_B	�fB	�zB	��B	��B	��B	��B	ŲB	��B	��B	��B	��B	��B	� B	��B	��B	��B	� B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�%B	�+B	�/B	�HB	�YB	�hB	�sB	�{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B
 
B
B
B
B
%B
+B
*B
0B
0B
2B
2B
2B
=B
	CB
PB
PB
TB
VB
gB
qB
�B
�B
%�B
+B
0*B
8\B
={B
A�B
G�B
L�B
R�B
Z(B
_EB
ddB
h|B
j�B
n�B
r�B
u�B
y�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.04 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311214432016053112144320160531121443  AO  ARCAADJP                                                                    20150503021528    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150503021528  QCP$                G�O�G�O�G�O�0               AO  ARGQQCPL                                                                    20150503021528  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531121443  IP                  G�O�G�O�G�O�                