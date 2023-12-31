CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-07-21T23:05:29Z AOML 3.0 creation; 2016-05-31T19:14:31Z UW 3.1 conversion     
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
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20140721230529  20160531121431  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               +A   AO  4051_7090_043                   2C  D   APEX                            5368                            041511                          846 @֟�mj�1   @֟��&��@51hr� ��d�;dZ�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    +A   A   A   @333@�  @���A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DMfDM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  DtY�DyFfD�  D�Y�D��3D�ٚD��D�C3D��fD���D��D�I�D�p D�ٚD�	�D�I�D�c3D���D��3D�I�D� D��3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @0��@}p�@��@��RA\)A?\)A_\)A\)A��A��A��A��AϮA߮A�A��B�
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
B�
B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D }qD �qD}qD�qD}qD�qDwD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD	}qD	�qD
}qD
�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�D}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD }qD �qD!}qD!�qD"}qD"�qD#}qD#�qD$}qD$�qD%}qD%�qD&}qD&�qD'}qD'�qD(}qD(�qD)}qD)�qD*}qD*�qD+}qD+�qD,}qD,�qD-}qD-�qD.}qD.�qD/}qD/�qD0}qD0�qD1}qD1�qD2}qD2�qD3}qD3�qD4}qD4�qD5}qD5�qD6}qD6�qD7}qD7�qD8}qD8�qD9}qD9�qD:}qD:�qD;}qD;�qD<}qD<�qD=}qD=�qD>}qD>�qD?}qD?�qD@}qD@�qDA}qDA�qDB}qDB�qDC}qDC�qDD}qDD�qDE}qDE�qDF}qDF�qDG}qDG�qDH}qDH�qDI}qDI�qDJ}qDJ�qDK}qDK�qDL}qDM�DM}qDM�qDN}qDN�qDO}qDO�qDP}qDP�qDQ}qDQ�qDR}qDR�qDS}qDS�qDT}qDT�qDU}qDU�qDV}qDV�qDW}qDW�qDX}qDX�qDY}qDY�qDZ}qDZ�qD[}qD[�qD\}qD\�qD]}qD]�qD^}qD^�qD_}qD_�qD`}qD`�qDa}qDa�qDb}qDb�qDc}qDc�qDd}qDd�qDe}qDe�qDf}qDf�qDg}qDg�qDh}qDh�qDi}qDi�qDj}qDj�qDk}qDk�qDl}qDl�qDm}qDm�qDn}qDn�qDo}qDo�qDp}qDp�qDq}qDq�qDr}qDr�qDs}qDs�qDtWDyC�D��D�XRD���D��RD��D�A�D��D���D��D�HRD�n�D��RD�RD�HRD�a�D�˅D���D�HRD�~�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��ÃȦ+A̋DA̋DA̋DA̋DA�K�A�E�Aơ�A��A���AőhA�oAć+A��mA�A�JA�x�A��A���A�t�A���A�9XA�  A��TA�?}A�hsA�A�K�A��PA�-A�/A�C�A��A��DA�VA�%A�ȴA�O�A��wA��A�I�A�A�l�A�ȴA���A�S�A���A���A�K�A��A��A��!A���A�r�A�1'A���A�K�A�A�ffA��A�G�A��A���A���A��+A���A��7A���A�A�A���A�ƨA���A��A�S�A�ĜA�S�A���A�33A���A��A�E�A�M�A�v�A�ZA���A���A�^5A���A��-A�/A�VA�$�A�A�p�A��PA�oA�O�A���A�%A�ffA}33A|  A{hsAy�AyoAx��Ax�\AvJAtbNArĜApAo33AohsAoXAm��Al�RAlz�AjE�Ah�RAgS�AfI�Ad5?Ab��AbA�Aa+A^�A]�A]A]��A]VAZ�AX�HAW�-AVjAT�`AS��AS\)AR9XAQC�APffAO"�AK"�AH�HAG%AF�\AE�PAD1AC��ACAB5?AA��AA�PAA�A?�A>��A=|�A;�FA:�A:$�A8��A7�#A6�RA5+A3/A1�hA1|�A1dZA0��A/C�A,r�A*��A*1A)A)C�A(�/A(�A'��A&�DA&=qA%�mA%A$ȴA#�A"A�A!�#A!�A�A��A �A��A�AbA��A  A�`A~�A  A%A�7A\)AbA\)A��A�+A5?AhsAoA|�A
�A
VA	�-Av�A��Al�AE�A�^A�AAz�A��A v�@��+@�  @��
@��@���@��;@��@�x�@�
=@�@��
@�~�@���@��@�@�9X@��@��@�@�F@�n�@�x�@���@�A�@�
=@�@�7L@�ƨ@�\)@�ff@ܬ@���@�;d@���@�@�l�@��@�v�@�$�@պ^@�z�@җ�@ѡ�@���@��@ϕ�@�o@��@���@�^5@���@̼j@�bN@���@�n�@�Q�@�ȴ@��@Ł@�7L@���@��@�  @���@�?}@��@��P@�V@�/@���@���@��F@���@���@���@�?}@�/@��u@�A�@�1@��@��@��@���@���@��-@��h@�X@�bN@�33@�{@��`@���@��P@�33@��\@��@��T@��#@�@�p�@�V@�(�@�`B@���@��@�1'@���@�33@���@���@���@�`B@���@���@��\@�"�@�n�@�@��+@��@�33@�-@��@�G�@�X@�7L@�7L@�hs@�`B@�X@�V@���@�A�@��
@��P@�\)@��\@��T@�p�@���@�z�@�r�@��`@��!@�~�@�hs@�Ĝ@�(�@�=q@�Ĝ@��@�;d@���@�;d@�J@��!@�\)@�ƨ@�I�@���@�%@���@�1'@��F@�9X@��9@�&�@��@��/@��/@���@�G�@��@���@���@�%@���@��@���@��D@�j@�Q�@�(�@��@��F@�;d@���@��@���@���@��9@���@�C�@�
=@��@���@���@�ff@�M�@�E�@�=q@��T@�X@�%@��@�Z@� �@�  @��m@�ƨ@���@�t�@��H@���@�v�@�=q@�-@�{@��^@��9@��u@�A�@��@�  @��@��
@��F@���@��@�l�@�C�@��@��!@�v�@�{@���@�G�@���@��@�9X@��@��@��@�b@�ƨ@���@��@�dZ@�o@��@���@���@��\@�v�@�ff@�M�@�=q@�$�@�@���@�7L@�V@�V@��`@��j@���@�S�@��F@z��@o�@jJ@cdZ@ZM�@R��@K33@B�H@<(�@5�-@/+@,z�@%�@!��@�@�P@@�@�/111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ÃȦ+A̋DA̋DA̋DA̋DA�K�A�E�Aơ�A��A���AőhA�oAć+A��mA�A�JA�x�A��A���A�t�A���A�9XA�  A��TA�?}A�hsA�A�K�A��PA�-A�/A�C�A��A��DA�VA�%A�ȴA�O�A��wA��A�I�A�A�l�A�ȴA���A�S�A���A���A�K�A��A��A��!A���A�r�A�1'A���A�K�A�A�ffA��A�G�A��A���A���A��+A���A��7A���A�A�A���A�ƨA���A��A�S�A�ĜA�S�A���A�33A���A��A�E�A�M�A�v�A�ZA���A���A�^5A���A��-A�/A�VA�$�A�A�p�A��PA�oA�O�A���A�%A�ffA}33A|  A{hsAy�AyoAx��Ax�\AvJAtbNArĜApAo33AohsAoXAm��Al�RAlz�AjE�Ah�RAgS�AfI�Ad5?Ab��AbA�Aa+A^�A]�A]A]��A]VAZ�AX�HAW�-AVjAT�`AS��AS\)AR9XAQC�APffAO"�AK"�AH�HAG%AF�\AE�PAD1AC��ACAB5?AA��AA�PAA�A?�A>��A=|�A;�FA:�A:$�A8��A7�#A6�RA5+A3/A1�hA1|�A1dZA0��A/C�A,r�A*��A*1A)A)C�A(�/A(�A'��A&�DA&=qA%�mA%A$ȴA#�A"A�A!�#A!�A�A��A �A��A�AbA��A  A�`A~�A  A%A�7A\)AbA\)A��A�+A5?AhsAoA|�A
�A
VA	�-Av�A��Al�AE�A�^A�AAz�A��A v�@��+@�  @��
@��@���@��;@��@�x�@�
=@�@��
@�~�@���@��@�@�9X@��@��@�@�F@�n�@�x�@���@�A�@�
=@�@�7L@�ƨ@�\)@�ff@ܬ@���@�;d@���@�@�l�@��@�v�@�$�@պ^@�z�@җ�@ѡ�@���@��@ϕ�@�o@��@���@�^5@���@̼j@�bN@���@�n�@�Q�@�ȴ@��@Ł@�7L@���@��@�  @���@�?}@��@��P@�V@�/@���@���@��F@���@���@���@�?}@�/@��u@�A�@�1@��@��@��@���@���@��-@��h@�X@�bN@�33@�{@��`@���@��P@�33@��\@��@��T@��#@�@�p�@�V@�(�@�`B@���@��@�1'@���@�33@���@���@���@�`B@���@���@��\@�"�@�n�@�@��+@��@�33@�-@��@�G�@�X@�7L@�7L@�hs@�`B@�X@�V@���@�A�@��
@��P@�\)@��\@��T@�p�@���@�z�@�r�@��`@��!@�~�@�hs@�Ĝ@�(�@�=q@�Ĝ@��@�;d@���@�;d@�J@��!@�\)@�ƨ@�I�@���@�%@���@�1'@��F@�9X@��9@�&�@��@��/@��/@���@�G�@��@���@���@�%@���@��@���@��D@�j@�Q�@�(�@��@��F@�;d@���@��@���@���@��9@���@�C�@�
=@��@���@���@�ff@�M�@�E�@�=q@��T@�X@�%@��@�Z@� �@�  @��m@�ƨ@���@�t�@��H@���@�v�@�=q@�-@�{@��^@��9@��u@�A�@��@�  @��@��
@��F@���@��@�l�@�C�@��@��!@�v�@�{@���@�G�@���@��@�9X@��@��@��@�b@�ƨ@���@��@�dZ@�o@��@���@���@��\@�v�@�ff@�M�@�=q@�$�@�@���@�7L@�V@�V@��`@��j@���@�S�@��F@z��@o�@jJ@cdZ@ZM�@R��@K33@B�H@<(�@5�-@/+@,z�@%�@!��@�@�P@@�@�/111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB  B  B  B  B  B��B�5B��BƨBǮBǮBĜB��B�jB�?B�!B��B��B��B��B�oB�\B�\B�\B�VB�JB�1B�1B�%B�B�Br�BiyBjBgmBaHBS�B?}B6FB/B+B$�B�B�BuB
=BB��B��B��B�B��B��BɺBƨB��B�?B�B��B��B��B�7B{�Bp�BaHBVBG�B5?B(�B#�B�B�B�B�BhBB�B�BB�
B��BÖB�FB��B�JB{�Bt�Bn�BgmBXB+B	7B
�sB
�B
ƨB
�'B
��B
�B
v�B
m�B
dZB
ZB
E�B
=qB
9XB
0!B
+B
(�B
%�B
�B
JB
B	�B	�B	�B	�B	�`B	�5B	�#B	��B	ŢB	�wB	�LB	�B	��B	��B	��B	�hB	�JB	�DB	�7B	�B	x�B	n�B	hsB	aHB	ZB	S�B	P�B	K�B	E�B	@�B	7LB	(�B	�B	�B	�B	"�B	�B	�B	�B	�B	�B	�B	�B	uB	VB	1B	B��B��B��B�B�B�NB�B��B��B��BȴB�}B�9B�'B�!B�!B�B�B�B�B��B��B��B��B��B��B��B��B��B�uB�hB�\B�PB�DB�1B�B�B� B}�Bz�Bw�Bt�Bq�Bn�Bm�Bl�Bk�BiyBffBdZBcTBbNBaHB`BB_;B^5B]/B^5B\)B[#B[#BYBW
BS�BQ�BQ�BQ�BP�BR�BR�BQ�BS�BS�BT�BVBW
BXBXBW
BXB\)B]/B]/B_;BbNBdZBe`Be`BgmBiyBjBm�Bn�Bm�Bm�Bo�Bp�Bq�Bq�Bv�Bv�Bw�Bw�Bw�Bw�Bx�Bu�Bu�Bu�Bu�Bt�Bs�Br�Bp�Bo�Bp�Bp�Bp�Br�Br�Bu�Bv�Bv�Bw�Bw�Bv�Bx�B{�B~�B~�B�B�+B�JB�hB�uB�uB�{B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�-B�^B��BÖBĜBƨB��B��B�B�
B�B�B�B�B�B�B�B�B�/B�HB�ZB�yB�B�B��B��B��B	DB	oB	�B	�B	�B	"�B	-B	0!B	.B	49B	8RB	:^B	;dB	=qB	>wB	?}B	@�B	@�B	@�B	A�B	A�B	@�B	>wB	>wB	=qB	<jB	<jB	@�B	H�B	YB	YB	W
B	W
B	XB	R�B	P�B	P�B	O�B	S�B	T�B	T�B	^5B	hsB	m�B	s�B	x�B	|�B	~�B	}�B	�B	�JB	�bB	�uB	�{B	��B	��B	��B	��B	�B	�B	�B	�B	�-B	�9B	�?B	�?B	�FB	�FB	�LB	�RB	�RB	�XB	�^B	�dB	�jB	�^B	�XB	�RB	�RB	�^B	�dB	�qB	�wB	��B	��B	��B	�}B	��B	ĜB	ƨB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�
B	�#B	�#B	�)B	�/B	�5B	�5B	�;B	�BB	�BB	�HB	�HB	�NB	�TB	�ZB	�ZB	�fB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B
%B
hB
�B
 �B
)�B
33B
9XB
=qB
D�B
K�B
O�B
S�B
YB
\)B
`BB
dZB
hsB
m�B
p�B
u�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B 
B 
B B B B��B�?B��BƲBǸBǸBħB��B�tB�JB�*B��B��B��B��B�yB�gB�gB�bB�^B�TB�9B�5B�0B�B�Br�Bi�Bj�BgqBaRBTB?�B6OB/ B+B$�B�B�B|B
@BB��B��B��B�B��B��B��BƭB��B�FB�B��B��B��B�8B{�Bp�BaMBV
BG�B5BB(�B#�B�B�B�B�BkB&B�B�GB�B��BÙB�HB��B�RB{�Bt�Bn�BgpBXB+B	=B
�zB
�B
ƭB
�1B
��B
�B
v�B
m�B
ddB
Z$B
E�B
=~B
9bB
0(B
+B
(�B
%�B
�B
WB
B	�B	�B	�B	�B	�mB	�DB	�.B	��B	ůB	��B	�ZB	�"B	��B	��B	��B	�yB	�YB	�SB	�GB	�*B	x�B	n�B	h�B	a[B	Z2B	TB	P�B	K�B	E�B	@�B	7^B	)	B	�B	�B	�B	"�B	�B	�B	�B	�B	�B	�B	�B	�B	lB	EB	!B�
B��B��B�B�B�gB�-B��B��B��B��B��B�RB�=B�8B�:B�/B�-B�%B�!B�B�B�B��B��B��B��B��B��B��B��B�uB�hB�^B�LB�8B�(B�B~
Bz�Bw�Bt�Bq�Bn�Bm�Bl�Bk�Bi�Bf�BdtBcqBbjBagB`_B_XB^QB]KB^PB\FB[@B[>BY2BW(BTBR
BR
BRBQBSBSBRBTBTBUBV"BW(BX.BX*BW'BX.B\FB]LB]JB_VBbgBdtBe|Be}Bg�Bi�Bj�Bm�Bn�Bm�Bm�Bo�Bp�Bq�Bq�Bv�Bv�Bw�Bw�Bw�Bw�Bx�Bu�Bu�Bu�Bu�Bt�Bs�Br�Bp�Bo�Bp�Bp�Bp�Br�Br�Bu�Bv�Bv�Bw�Bw�Bv�Bx�B|BBB�'B�FB�dB��B��B��B��B��B��B��B��B��B��B��B�B�!B�!B�!B�!B�$B�,B�.B�CB�wB��BëBĳBƾB��B�B�B� B�&B�&B�-B�-B�B�B�-B�3B�GB�^B�pB�B�B�B��B��B�B	XB	�B	�B	�B	�B	"�B	-!B	02B	.(B	4MB	8cB	:pB	;wB	=�B	>�B	?�B	@�B	@�B	@�B	A�B	A�B	@�B	>�B	>�B	=�B	<yB	<yB	@�B	H�B	Y(B	Y(B	WB	WB	XB	SB	P�B	P�B	O�B	TB	UB	UB	^GB	h�B	m�B	s�B	x�B	|�B		B	~B	�!B	�ZB	�rB	��B	��B	��B	��B	��B	��B	�B	�B	�B	�"B	�;B	�HB	�LB	�MB	�SB	�TB	�ZB	�aB	�`B	�gB	�kB	�sB	�yB	�mB	�fB	�bB	�_B	�kB	�rB	�B	��B	��B	��B	��B	��B	��B	ĭB	ƳB	ǻB	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�B	�B	�B	�B	�/B	�/B	�6B	�<B	�BB	�AB	�GB	�MB	�NB	�UB	�TB	�ZB	�aB	�eB	�fB	�tB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B
2B
tB
�B
 �B
*B
3?B
9aB
=~B
D�B
K�B
O�B
TB
YB
\2B
`OB
dbB
hzB
m�B
p�B
u�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.04 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311214312016053112143120160531121431  AO  ARCAADJP                                                                    20140721230529    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721230529  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721230529  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531121431  IP                  G�O�G�O�G�O�                