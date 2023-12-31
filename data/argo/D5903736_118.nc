CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-07-14T02:15:18Z AOML 3.0 creation; 2016-05-31T19:14:44Z UW 3.1 conversion     
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
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20150714021518  20160531121444  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               vA   AO  4051_7090_118                   2C  D   APEX                            5368                            041511                          846 @�_�ff?�1   @�_��� @3�n��P�dV^5?|�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    vA   A   A   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A���A�  A�  B   B  B  B  B   B(  B0  B8  B@  BHffBPffBX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�33B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4�C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Cg�fCj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�3Dy��D�	�D�I�D�p D��3D���D�L�D��3D��3D�fD�@ D�Y�D�ٚD�	�D�FfDڜ�D��fD�	�D�33D�\�D��3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�Q�@��R@��RA\)A?\)A_\)A\)A��A��A��A��A�z�A߮A�A��B�
B�
B�
B�
B'�
B/�
B7�
B?�
BH=pBP=pBW�
B_�
Bg�
Bo�
Bw�
B�
B��B��B��B��B��B��RB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C4]C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg�)Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D }qD �qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD	}qD	�qD
}qD
�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD }qD �qD!}qD!�qD"}qD"�qD#}qD#�qD$}qD$�qD%}qD%�qD&}qD&�qD'}qD'�qD(}qD(�qD)}qD)�qD*}qD*�qD+}qD+�qD,}qD,�qD-}qD-�qD.}qD.�qD/}qD/�qD0}qD0�qD1}qD1�qD2}qD2�qD3}qD3�qD4}qD4�qD5}qD5�qD6}qD6�qD7}qD7�qD8}qD8�qD9}qD9�qD:}qD:�qD;}qD;�qD<}qD<�qD=}qD=�qD>}qD>�qD?}qD?�qD@}qD@�qDA}qDA�qDB}qDB�qDC}qDC�qDD}qDD�qDE}qDE�qDF}qDF�qDG}qDG�qDH}qDH�qDI}qDI�qDJ}qDJ�qDK}qDK�qDL}qDL�qDM}qDM�qDN}qDN�qDO}qDO�qDP}qDP�qDQ}qDQ�qDR}qDR�qDS}qDS�qDT}qDT�qDU}qDU�qDV}qDV�qDW}qDW�qDX}qDX�qDY}qDY�qDZ}qDZ�qD[}qD[�qD\}qD\�qD]}qD]�qD^}qD^�qD_}qD_�qD`}qD`�qDa}qDa�qDb}qDb�qDc}qDc�qDd}qDd�qDe}qDe�qDf}qDf�qDg}qDg�qDh}qDh�qDi}qDi�qDj}qDj�qDk}qDk�qDl}qDl�qDm}qDm�qDn}qDn�qDo}qDo�qDp}qDp�qDq}qDq�qDr}qDr�qDs}qDs�qDt}qDtФDy�D�RD�HRD�n�D���D��RD�K�D���D���D�D�>�D�XRD��RD�RD�EDڛ�D��D�RD�1�D�[�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��TA��TA��`A��`A��mA��HA�ƨAܝ�A�(�A��AۼjA�l�A�
=Aח�A��AՕ�A�S�A���A�Q�A�r�A�1'A���A�
=AӬA�G�A�ƨA��
A�S�A�oA̓uA�1'A˝�A�O�A���A��A�n�A��TA��mAœuA�E�Aĉ7A�\)A�9XA��A���A���A�A�A�x�A���A��A�9XA� �A�{A���A�\)A�1A�S�A���A�I�A��hA�;dA��`A�7LA��7A�7LA��A��A��+A��yA�=qA�1A��A��7A��FA�`BA���A��A��HA���A�bA���A��A��RA�dZA�?}A��mA�&�A��A�^5A�l�A���A��A���A�\)A���A� �A��A���A���A���A���A�7LA��^A�ȴA��!A���A��A���A��A�A��uA��+A��A�"�A���A���A���A�O�A�hsA�%A��A���A�$�A~r�A{��Ay
=Av��Ar��Ao�Aj�jAi33Ag�#Ae�-Aa��A^�\A]x�A[��A[`BAY�AXjAV��AUoAS��ARQ�AP-AMAJ9XAI��AHȴAG��AG`BAF�AF  AE7LAC��ABȴABjAAO�A?|�A>M�A<ȴA;|�A8��A6�`A5��A4�A3�A2��A1p�A/�mA-��A,��A,E�A+��A+;dA*�!A)��A)x�A(A�A't�A%��A#?}A!;dA�jA&�AVA�HA�An�A\)A9XA\)A��AffAJA��A{A`BA�`AĜAZA"�A�AG�A~�A$�AAƨA
�9A	`BA�FA��A�hA�A/A��A��A�A�#AhsA =q@�l�@��H@�/@�33@��@���@���@�33@�M�@���@���@� �@��m@��y@�ff@���@�@�E�@홚@�p�@�u@�b@�^5@�V@���@��@�@�p�@�7@�^@��@�@�@���@ߝ�@�M�@���@�~�@�@�v�@�(�@ύP@�hs@˶F@��@� �@��;@�C�@�@ļj@�l�@�@�Ĝ@�|�@�5?@���@�C�@�?}@�Ĝ@�r�@���@�@�~�@��7@�V@��9@�Z@��\@��T@���@�p�@���@�5?@�-@�@�/@�Ĝ@�9X@��@�33@��y@��!@��R@��@��@�~�@���@��+@�n�@���@��R@���@��\@�J@��#@��@�S�@�ff@��^@��h@���@�7L@��u@�t�@��y@�+@�|�@��@��P@���@��m@�Q�@�1@�l�@�9X@���@��@���@�z�@�(�@�  @�dZ@��+@�p�@��j@���@�I�@�1'@��;@�K�@�K�@�t�@���@�r�@��u@��D@��@���@�dZ@�;d@�C�@�o@��@�~�@�=q@�@�@���@��h@�X@�/@��@���@��`@���@���@�bN@� �@��w@��P@�\)@�\)@�C�@���@�M�@�{@�@�5?@�$�@�@��@�x�@��@���@���@�j@�9X@��;@�K�@��y@��+@�ff@�V@�=q@��@��@�@�G�@�/@�7L@�?}@�%@��9@��D@��@�z�@�r�@��m@�;d@�;d@�33@��@��y@��R@�~�@�5?@��T@�@�@��-@�p�@�X@��@�r�@�bN@�Q�@��m@��F@�l�@�dZ@�S�@�K�@�33@�o@���@��!@�=q@�-@�$�@��@�@��@��#@��#@��^@���@�hs@��u@�j@�(�@��@���@�dZ@�33@��@��H@���@��R@��!@���@�ff@�E�@��^@��@�X@�?}@�Ĝ@�j@�9X@�1@�  @��@��m@���@���@�o@��!@�v�@��/@�1'@v��@k�F@d��@^5?@T��@L��@E��@?�w@9�@3C�@,z�@%��@5?@��@j@��@$�@dZ@�R111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��TA��TA��`A��`A��mA��HA�ƨAܝ�A�(�A��AۼjA�l�A�
=Aח�A��AՕ�A�S�A���A�Q�A�r�A�1'A���A�
=AӬA�G�A�ƨA��
A�S�A�oA̓uA�1'A˝�A�O�A���A��A�n�A��TA��mAœuA�E�Aĉ7A�\)A�9XA��A���A���A�A�A�x�A���A��A�9XA� �A�{A���A�\)A�1A�S�A���A�I�A��hA�;dA��`A�7LA��7A�7LA��A��A��+A��yA�=qA�1A��A��7A��FA�`BA���A��A��HA���A�bA���A��A��RA�dZA�?}A��mA�&�A��A�^5A�l�A���A��A���A�\)A���A� �A��A���A���A���A���A�7LA��^A�ȴA��!A���A��A���A��A�A��uA��+A��A�"�A���A���A���A�O�A�hsA�%A��A���A�$�A~r�A{��Ay
=Av��Ar��Ao�Aj�jAi33Ag�#Ae�-Aa��A^�\A]x�A[��A[`BAY�AXjAV��AUoAS��ARQ�AP-AMAJ9XAI��AHȴAG��AG`BAF�AF  AE7LAC��ABȴABjAAO�A?|�A>M�A<ȴA;|�A8��A6�`A5��A4�A3�A2��A1p�A/�mA-��A,��A,E�A+��A+;dA*�!A)��A)x�A(A�A't�A%��A#?}A!;dA�jA&�AVA�HA�An�A\)A9XA\)A��AffAJA��A{A`BA�`AĜAZA"�A�AG�A~�A$�AAƨA
�9A	`BA�FA��A�hA�A/A��A��A�A�#AhsA =q@�l�@��H@�/@�33@��@���@���@�33@�M�@���@���@� �@��m@��y@�ff@���@�@�E�@홚@�p�@�u@�b@�^5@�V@���@��@�@�p�@�7@�^@��@�@�@���@ߝ�@�M�@���@�~�@�@�v�@�(�@ύP@�hs@˶F@��@� �@��;@�C�@�@ļj@�l�@�@�Ĝ@�|�@�5?@���@�C�@�?}@�Ĝ@�r�@���@�@�~�@��7@�V@��9@�Z@��\@��T@���@�p�@���@�5?@�-@�@�/@�Ĝ@�9X@��@�33@��y@��!@��R@��@��@�~�@���@��+@�n�@���@��R@���@��\@�J@��#@��@�S�@�ff@��^@��h@���@�7L@��u@�t�@��y@�+@�|�@��@��P@���@��m@�Q�@�1@�l�@�9X@���@��@���@�z�@�(�@�  @�dZ@��+@�p�@��j@���@�I�@�1'@��;@�K�@�K�@�t�@���@�r�@��u@��D@��@���@�dZ@�;d@�C�@�o@��@�~�@�=q@�@�@���@��h@�X@�/@��@���@��`@���@���@�bN@� �@��w@��P@�\)@�\)@�C�@���@�M�@�{@�@�5?@�$�@�@��@�x�@��@���@���@�j@�9X@��;@�K�@��y@��+@�ff@�V@�=q@��@��@�@�G�@�/@�7L@�?}@�%@��9@��D@��@�z�@�r�@��m@�;d@�;d@�33@��@��y@��R@�~�@�5?@��T@�@�@��-@�p�@�X@��@�r�@�bN@�Q�@��m@��F@�l�@�dZ@�S�@�K�@�33@�o@���@��!@�=q@�-@�$�@��@�@��@��#@��#@��^@���@�hs@��u@�j@�(�@��@���@�dZ@�33@��@��H@���@��R@��!@���@�ff@�E�@��^@��@�X@�?}@�Ĝ@�j@�9X@�1@�  @��@��m@���@���@�o@��!@�v�@��/@�1'@v��@k�F@d��@^5?@T��@L��@E��@?�w@9�@3C�@,z�@%��@5?@��@j@��@$�@dZ@�R111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
	7B
	7B
	7B
	7B
	7B
	7B

=B
JB
uB
{B
�B
{B
1B	��B
B
7LB
s�B
~�B�B5?B;dB;dB0!B2-B5?BC�B:^B'�B{B9XBH�BffBu�B�JB�B��B�#B{B6FB;dBF�B`BBaHBbNBdZBw�Bx�B�1B��B��B�!B�3BÖBȴB��B��B��B�/B�#B�#B�B�B��BB1B	7B	7B	7BVB�B�B�B �B�B�BoB��B��B��B�TB�/B�HB�TB�B��B�B�sB�B�dB��BN�BuB�
B�'B��B�7Bn�BaHBH�B(�B�B
��B
�fB	7BZBl�Bw�Bu�BW
B$�BB
��BB
��B
�B
�}B
�B
��B
�uB
|�B
[#B
;dB
7LB
-B
"�B
hB	��B	�;B	B	��B	��B	�bB	� B	m�B	aHB	]/B	T�B	Q�B	H�B	@�B	:^B	2-B	+B	 �B	�B	
=B��B��B��B��B�B�B�B�sB�TB�5B�#B�B��B��BȴBĜB�wB�RB�?B�-B�!B�B�9B�FBŢBǮB��B��B��B��B��B��B��B��B��BÖB�3B��B��B��B��B�oB�hB�oB�hB�VB�\B�bB�bB�bB�VB�JB�PB�PB�JB�JB�VB�DB�=B�JB�\B�bB�bB�DB�+B�B�Bx�By�By�Bx�Bw�Bv�Bw�Bx�By�Bz�By�Bx�B|�B|�B|�B|�B}�B}�B|�B|�B|�Bz�Bx�Bz�B{�Bz�B{�B{�B|�B|�B|�B|�B}�B}�B�B�DB�JB�VB�{B��B�uB�hB�\B�PB�=B�Bv�BiyBffBr�Bv�Bu�Bs�Bu�Bv�Bw�Bz�B{�B}�B� B�B�B�%B�=B�VB�hB�hB�oB��B��B��B��B��B��B��B�B�B�B�B�?B�jB�jB�qB��B��BÖBǮB��B��B��B��B�
B�/B�ZB�B�B��B��B��B��B	
=B	�B	�B	�B	�B	�B	�B	�B	!�B	"�B	!�B	"�B	%�B	,B	2-B	9XB	8RB	:^B	=qB	B�B	B�B	F�B	S�B	aHB	cTB	e`B	ffB	hsB	iyB	iyB	k�B	n�B	q�B	t�B	u�B	w�B	y�B	{�B	}�B	� B	�B	�7B	�PB	�\B	�bB	�oB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�'B	�3B	�3B	�3B	�9B	�?B	�FB	�RB	�^B	�dB	�jB	��B	��B	ÖB	ĜB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�
B	�
B	�B	�#B	�;B	�BB	�HB	�NB	�TB	�ZB	�fB	�mB	�sB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
B
%B
1B
1B

=B

=B
DB
DB
DB
DB
DB
DB
DB
JB
PB
\B
bB
�B
!�B
)�B
1'B
6FB
>wB
B�B
I�B
M�B
S�B
XB
^5B
dZB
k�B
o�B
s�B
w�B
y�B
{�B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
	CB
	CB
	@B
	@B
	AB
	CB

HB
TB
�B
�B
�B
�B
=B	��B
B
7TB
s�B
B�B5DB;fB;kB0(B21B5CBC�B:cB'�B�B9[BH�BfkBu�B�LB�	B��B�$B}B6MB;hBF�B`GBaIBbTBd^Bw�Bx�B�5B��B��B�%B�7BÜBȺB��B��B��B�5B�&B�'B�B�B��BB7B	;B	:B	=BVB�B�B�B �B�B�BvB��B��B��B�XB�4B�LB�YB�B��B�B�zB�B�gB��BN�BxB�B�-B��B�;Bn�BaLBH�B(�B�B
��B
�lB	=BZ#Bl�Bw�Bu�BWB$�B%B
��B
B
��B
�$B
��B
�B
��B
�|B
|�B
[*B
;nB
7WB
-B
"�B
uB	�B	�HB	B	��B	��B	�sB	�B	m�B	a[B	]@B	UB	Q�B	H�B	@�B	:tB	2?B	+B	 �B	�B	
QB�B��B��B��B��B�B�B�B�lB�LB�9B�B��B��B��BĵB��B�hB�XB�EB�9B�'B�QB�_BźB��B��B��B��B��B��B��B��B��B��BïB�HB��B��B��B��B��B�B��B��B�nB�wB�zB�|B�}B�oB�dB�kB�iB�dB�dB�pB�_B�VB�eB�vB�}B�~B�_B�EB�+B� Bx�By�By�Bx�Bw�Bv�Bw�Bx�By�Bz�By�Bx�B}	B}	B}	B}B~B~B}B}	B}Bz�Bx�Bz�B|Bz�B|B|B}B}	B}	B}B~B~B�(B�]B�cB�qB��B��B��B��B�wB�hB�WB�-Bv�Bi�Bf�Br�Bv�Bu�Bs�Bu�Bv�Bw�Bz�B|B~B�B�#B�5B�=B�UB�nB��B��B��B��B��B��B��B��B�	B�	B�'B�-B�2B�-B�VB��B��B��B��B��BïB��B��B��B��B�	B� B�DB�oB�B�B��B��B�B�B	
PB	�B	�B	�B	�B	�B	�B	�B	!�B	"�B	!�B	"�B	%�B	,B	2?B	9iB	8bB	:qB	=�B	B�B	B�B	F�B	TB	aZB	cdB	eqB	fuB	h�B	i�B	i�B	k�B	n�B	q�B	t�B	u�B	w�B	y�B	{�B	~B	�B	�*B	�FB	�_B	�nB	�sB	�}B	�~B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�(B	�.B	�4B	�CB	�BB	�BB	�GB	�LB	�TB	�`B	�kB	�sB	�wB	��B	��B	âB	ĨB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�0B	�GB	�PB	�UB	�ZB	�bB	�fB	�rB	�xB	�B	�yB	�~B	�B	�B	�B	�B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B
B
B
B
B
B
B
B
B
B
B
$B
1B
>B
>B

JB

JB
OB
RB
PB
PB
PB
PB
OB
WB
[B
iB
lB
�B
!�B
*B
1/B
6OB
>B
B�B
I�B
M�B
TB
XB
^>B
dbB
k�B
o�B
s�B
w�B
y�B
{�B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.04 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311214442016053112144420160531121444  AO  ARCAADJP                                                                    20150714021518    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150714021518  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20150714021518  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531121444  IP                  G�O�G�O�G�O�                