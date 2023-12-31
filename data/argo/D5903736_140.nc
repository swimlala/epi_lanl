CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2016-02-24T10:16:12Z AOML 3.0 creation; 2016-05-31T19:14:48Z UW 3.1 conversion     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         (Matthew Alkire, University of Washington      @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7$   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7(   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    7,   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7<   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7L   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7\   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7d   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    8   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    8   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     8    DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8@   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8D   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8H   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8h   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8�   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8�   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~       axis      T           8�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
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
_FillValue                    �    HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �P   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �`   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �d   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �t   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �x   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �|   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20160224101612  20190604094000  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  4051_7090_140                   2C  D   APEX                            5368                            041511                          846 @ט�h��1   @ט1�d.@4�I�^5�dX�hr�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   A   A   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CG�fCJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^�C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD�fD   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DM��DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt��Dy� D���D�6fD�y�D��3D��D�6fD�\�D��fD��D�FfD��3DǬ�D�fD�@ Dڃ3D�ɚD�	�D�FfD� D��f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��R@��RA\)A?\)A_\)A\)A��A��A��A��AϮA߮A�A��B�
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
B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG�)CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C^]C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D }qD �qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD	}qD	�qD
}qD
�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�D��D�qD }qD �qD!}qD!�qD"}qD"�qD#}qD#�qD$}qD$�qD%}qD%�qD&}qD&�qD'}qD'�qD(}qD(�qD)}qD)�qD*}qD*�qD+}qD+�qD,}qD,�qD-}qD-�qD.}qD.�qD/}qD/�qD0}qD0�qD1}qD1�qD2}qD2�qD3}qD3�qD4}qD4�qD5}qD5�qD6}qD6�qD7}qD7�qD8}qD8�qD9}qD9�qD:}qD:�qD;}qD;�qD<}qD<�qD=}qD=�qD>}qD>�qD?}qD?�qD@}qD@�qDA}qDA�qDB}qDB�qDC}qDC�qDD}qDD�qDE}qDE�qDF}qDF�qDG}qDG�qDH}qDH�qDI}qDI�qDJ}qDJ�qDK}qDK�qDL}qDL�qDM}qDM�DN}qDN�qDO}qDO�qDP}qDP�qDQ}qDQ�qDR}qDR�qDS}qDS�qDT}qDT�qDU}qDU�qDV}qDV�qDW}qDW�qDX}qDX�qDY}qDY�qDZ}qDZ�qD[}qD[�qD\}qD\�qD]}qD]�qD^}qD^�qD_}qD_�qD`}qD`�qDa}qDa�qDb}qDb�qDc}qDc�qDd}qDd�qDe}qDe�qDf}qDf�qDg}qDg�qDh}qDh�qDi}qDi�qDj}qDj�qDk}qDk�qDl}qDl�qDm}qDm�qDn}qDn�qDo}qDo�qDp}qDp�qDq}qDq�qDr}qDr�qDs}qDs�qDt}qDt�>Dy�qD��RD�5D�xRD���D��D�5D�[�D��D�RD�ED���Dǫ�D�D�>�Dځ�D��RD�RD�ED�~�D��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��Aʡ�AʅA�x�A�hsA�bNA�^5A�ZA�VA�S�A�Q�A�Q�A�O�A�M�A�K�A�I�A�I�A�I�A�I�A�K�A�K�A�K�A�I�A�I�A�G�A�I�A�I�A�G�A�G�A�I�A�O�AʁA��mA��AȲ-A�l�A�$�A��A���A�1'AƗ�A�(�A�hsAġ�A�bNA�&�AìA�O�A�1A�x�A�oA���A�VA��A���A��A� �A��A���A���A�1A��A��A���A�
=A��DA�O�A�ZA���A�33A�t�A���A���A���A���A�O�A�A��PA�JA�A�n�A�K�A���A�z�A��hA�^5A�  A�;dA��A���A�oA���A���A�"�A�;dA�A�=qA�|�A���A��A�z�A�v�A�p�A���A�VA�{A��;A�XA�JA�ZA��!A���A��A�r�A�bNA�1'A�dZA�K�A&�A}�;Az��Ay�Aw��Av�Av  At��Asl�Ap=qAm�hAk��Ai�Ag�Af��Ae�hAc�;Ab^5A`VA^��A]?}A[��AZ��AY`BAX^5AV�AUK�AS+AP�AP�AO�#AOAO�-AO`BAL�jAK33AI�wAH�uAGl�AF9XAE��AD��AB��A@A�A=�FA;/A9��A8��A6�!A5|�A4�`A4I�A3oA1/A0�jA0JA.�A-�7A*ZA)�mA'�A&=qA%7LA$-A#�A#?}A"��A"1A��AZAAJA��A|�AVA�-A�A-A?}A �A��A"�A9XAl�A�A�
A��A$�Ax�A
�!A
9XA	�^A	K�A�A	��A	�
A	&�AjA��A=qA�!Al�A�A ��A �A A�@���@��7@���@�5?@�j@�P@�+@�E�@�-@��@��@�;d@�1'@��@�ȴ@�^@��@��#@��@�  @�+@��@�{@�Q�@�-@�G�@؛�@�o@�`B@�%@��m@�
=@���@��T@��@�+@Η�@�/@�b@˶F@�\)@���@�=q@ɲ-@ȴ9@��@�ƨ@�@�V@��#@��@ēu@�1'@öF@��@�M�@���@��@�1'@�$�@���@�@���@��/@�z�@�b@���@�l�@��@�5?@�`B@���@���@��@�\)@��!@�$�@��@�V@���@��9@��@���@��7@��`@��@�j@� �@���@�|�@�S�@�ȴ@�E�@��@���@�G�@��@��@��/@��`@��u@�I�@�9X@� �@���@��w@�K�@���@���@��\@�=q@���@��@��T@���@���@�hs@�`B@�O�@�?}@��@�%@���@��j@��u@��@�bN@� �@�C�@���@��\@��H@��y@��R@�E�@���@��@�{@�@�p�@��@��`@��D@�Q�@�  @��
@�dZ@���@��h@��@��9@��@�1'@��m@�|�@��@�5?@�@�G�@���@���@��`@���@��9@��9@�Z@���@��@��m@�  @��
@���@��P@�|�@�l�@�\)@�+@��@�o@���@�E�@�@��T@��#@�@���@�X@�/@���@��u@�I�@��@��;@���@�|�@�t�@�"�@�@��@��@��+@�=q@��@���@��T@���@��@�&�@���@�Q�@�1'@���@��F@��@�\)@��@�l�@�;d@�J@��h@�x�@�?}@��@��@��/@���@���@��9@�r�@�1@� �@�1'@�I�@�I�@�1'@��
@�dZ@�33@�o@���@���@���@�ff@��@���@��h@�&�@��@�A�@�1'@�  @��P@�
=@��y@��H@���@���@�v�@�V@�J@��-@�O�@��@�Q�@��@��
@�&�@���@|1@q7L@fff@^ȴ@W�w@R�\@H��@B��@<�@5`B@/�@(�9@#��@�@�@�T@�!@@	G�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  Aʡ�AʅA�x�A�hsA�bNA�^5A�ZA�VA�S�A�Q�A�Q�A�O�A�M�A�K�A�I�A�I�A�I�A�I�A�K�A�K�A�K�A�I�A�I�A�G�A�I�A�I�A�G�A�G�A�I�A�O�AʁA��mA��AȲ-A�l�A�$�A��A���A�1'AƗ�A�(�A�hsAġ�A�bNA�&�AìA�O�A�1A�x�A�oA���A�VA��A���A��A� �A��A���A���A�1A��A��A���A�
=A��DA�O�A�ZA���A�33A�t�A���A���A���A���A�O�A�A��PA�JA�A�n�A�K�A���A�z�A��hA�^5A�  A�;dA��A���A�oA���A���A�"�A�;dA�A�=qA�|�A���A��A�z�A�v�A�p�A���A�VA�{A��;A�XA�JA�ZA��!A���A��A�r�A�bNA�1'A�dZA�K�A&�A}�;Az��Ay�Aw��Av�Av  At��Asl�Ap=qAm�hAk��Ai�Ag�Af��Ae�hAc�;Ab^5A`VA^��A]?}A[��AZ��AY`BAX^5AV�AUK�AS+AP�AP�AO�#AOAO�-AO`BAL�jAK33AI�wAH�uAGl�AF9XAE��AD��AB��A@A�A=�FA;/A9��A8��A6�!A5|�A4�`A4I�A3oA1/A0�jA0JA.�A-�7A*ZA)�mA'�A&=qA%7LA$-A#�A#?}A"��A"1A��AZAAJA��A|�AVA�-A�A-A?}A �A��A"�A9XAl�A�A�
A��A$�Ax�A
�!A
9XA	�^A	K�A�A	��A	�
A	&�AjA��A=qA�!Al�A�A ��A �A A�@���@��7@���@�5?@�j@�P@�+@�E�@�-@��@��@�;d@�1'@��@�ȴ@�^@��@��#@��@�  @�+@��@�{@�Q�@�-@�G�@؛�@�o@�`B@�%@��m@�
=@���@��T@��@�+@Η�@�/@�b@˶F@�\)@���@�=q@ɲ-@ȴ9@��@�ƨ@�@�V@��#@��@ēu@�1'@öF@��@�M�@���@��@�1'@�$�@���@�@���@��/@�z�@�b@���@�l�@��@�5?@�`B@���@���@��@�\)@��!@�$�@��@�V@���@��9@��@���@��7@��`@��@�j@� �@���@�|�@�S�@�ȴ@�E�@��@���@�G�@��@��@��/@��`@��u@�I�@�9X@� �@���@��w@�K�@���@���@��\@�=q@���@��@��T@���@���@�hs@�`B@�O�@�?}@��@�%@���@��j@��u@��@�bN@� �@�C�@���@��\@��H@��y@��R@�E�@���@��@�{@�@�p�@��@��`@��D@�Q�@�  @��
@�dZ@���@��h@��@��9@��@�1'@��m@�|�@��@�5?@�@�G�@���@���@��`@���@��9@��9@�Z@���@��@��m@�  @��
@���@��P@�|�@�l�@�\)@�+@��@�o@���@�E�@�@��T@��#@�@���@�X@�/@���@��u@�I�@��@��;@���@�|�@�t�@�"�@�@��@��@��+@�=q@��@���@��T@���@��@�&�@���@�Q�@�1'@���@��F@��@�\)@��@�l�@�;d@�J@��h@�x�@�?}@��@��@��/@���@���@��9@�r�@�1@� �@�1'@�I�@�I�@�1'@��
@�dZ@�33@�o@���@���@���@�ff@��@���@��h@�&�@��@�A�@�1'@�  @��P@�
=@��y@��H@���@���@�v�@�V@�J@��-@�O�@��@�Q�@��@��
@�&�@���@|1@q7L@fff@^ȴ@W�w@R�\@H��@B��@<�@5`B@/�@(�9@#��@�@�@�T@�!@@	G�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBx�Bx�Bx�Bx�Bx�Bx�Bx�Bx�Bx�Bx�Bx�Bx�Bx�Bx�Bx�Bx�Bx�Bx�Bx�Bx�Bx�Bx�By�By�By�By�By�B{�B|�B�B��Bo�B�hB�BŢB��B�B�5B�B�B��B��B��B��BB
=B
=BDBoB�B�B&�B-B/B/B.B%�B"�B�B�B�BJBBBB��B��B��B��B�B�B�fB�BB�#B�B��B��B��BB�B��By�Bn�BVBE�B?}B8RB33B�BuB  B�5BƨB�LB�!B��B��B��B�\B�7Bz�Bk�BQ�B?}B:^B33B�BDB
��B
�B
�B
�BB
ǮB
��B
��B
B
�'B
��B
��B
z�B
m�B
dZB
[#B
VB
K�B
:^B
�B
1B	��B	�`B	�#B	��B	��B	�qB	�!B	��B	�bB	�+B	~�B	w�B	o�B	ffB	\)B	R�B	F�B	>wB	=qB	;dB	:^B	9XB	5?B	%�B	�B	�B	bB	DB	+B	B	B��B�B�NB�B��B��B��BɺBɺBǮBŢBƨBŢBÖB�^B�?B�3B�?B�'B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B��B��B��B��B�\B�=B�B�B�B� B}�B{�Bz�Bx�Bw�Bx�Bx�B}�B�B��B��B��B��B��B�oB�JB�JB�PB�bB�\B�DB�=B� Bw�B�7B�+B�=B�PB�PB�DB�=B�%B�B}�B{�B|�B{�Bw�Bs�Bs�Bu�Bw�B|�B|�Bz�B{�B�B�+B�+B�PB�VB�oB��B��B��B��B��B��B��B��B��B��B��B��B��B�B�!B�!B�'B�3B�9B�LB�^B�qB��BŢBȴBȴBȴB��B��B�B�B�B�)B�/B�5B�5B�HB�B�B�B��B��B	B	B	1B	JB	VB	hB	uB	{B	�B	�B	 �B	$�B	'�B	+B	-B	.B	1'B	2-B	6FB	9XB	;dB	=qB	@�B	A�B	B�B	C�B	C�B	F�B	H�B	H�B	H�B	I�B	J�B	L�B	P�B	P�B	P�B	S�B	VB	VB	W
B	XB	ZB	\)B	]/B	^5B	^5B	_;B	`BB	bNB	cTB	e`B	e`B	ffB	hsB	k�B	n�B	r�B	u�B	w�B	w�B	w�B	w�B	|�B	� B	�%B	�1B	�7B	�PB	�bB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�'B	�-B	�3B	�XB	�^B	�qB	B	ĜB	ƨB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�B	�B	�B	�B	�B	�B	�)B	�)B	�/B	�5B	�BB	�NB	�`B	�fB	�fB	�fB	�mB	�sB	�sB	�sB	�sB	�sB	�mB	�sB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
B
B
B
B
B
+B
DB
DB
�B
$�B
-B
1'B
7LB
9XB
?}B
E�B
H�B
N�B
VB
\)B
bNB
ffB
jB
o�B
r�B
w�B
{�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  By�By�By�By�By�By�By�By�By�By�By�By�By�By�By�By�By�By�By�By�By�By�Bz�Bz�Bz�Bz�Bz�B|�B}�B��B��Bp�B�ZB�BƒB��B�B�'B�sB�B��B��B��B��B�B2B+B7B^BwB�B'�B-�B0B0B/B&�B#�B�B�B{B=BBB�B��B��B��B��B�B�{B�WB�2B�B�B��B��B��BÄB��B��Bz�Bo�BV�BF�B@mB9AB4#B�BaB �B�&BǙB�;B�B��B��B��B�IB�#B{�BlvBR�B@nB;NB4B�B0B
��B
��B
�B
�.B
țB
ͻB
��B
�|B
�B
��B
�sB
{�B
n}B
eCB
\B
V�B
L�B
;HB
�B
	B	��B	�NB	�B	��B	̲B	�`B	�B	��B	�OB	�B	�B	x�B	p�B	gRB	]B	S�B	G�B	?`B	>ZB	<QB	;IB	:CB	6+B	&�B	�B	jB	MB	,B	B	B	�B��B�rB�6B�B��B��B̴BʥBʡBȚBƎBǑBƋB�|B�FB�*B�B�+B�B��B��B��B��B��B��B��B��B��B��B�tB�wB�qB�gB�nB�wB��B�uB�GB�'B�	B��B��B��B~�B|�B{�By�Bx�By�By�B~�B� B��B��B��B��B��B�ZB�5B�4B�:B�LB�EB�/B�(B��Bx�B�"B�B�)B�6B�;B�+B�(B�B��B~�B|�B}�B|�Bx�Bt�Bt�Bv�Bx�B}�B}�B{�B|�B��B�B�B�9B�EB�ZB�iB�iB�pB��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B� B�5B�DB�]B�tBƍBɛBɛBɝB̱B��B��B��B�B�B�B� B�B�2B�hB�B��B��B��B	�B	�B		B	0B	>B	UB	]B	eB	�B	�B	!�B	%�B	(�B	+�B	-�B	.�B	2B	3B	71B	:@B	<NB	>XB	AlB	BtB	CyB	D�B	D�B	G�B	I�B	I�B	I�B	J�B	K�B	M�B	Q�B	Q�B	Q�B	T�B	V�B	V�B	W�B	X�B	[B	]B	^B	_B	_ B	`(B	a,B	c;B	d<B	fIB	fKB	gTB	i^B	loB	o�B	s�B	v�B	x�B	x�B	x�B	x�B	}�B	��B	�B	�B	�!B	�;B	�KB	�aB	�wB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�@B	�IB	�\B	�{B	ŉB	ǓB	ɟB	ʤB	̲B	̱B	ͶB	ͻB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	�B	�B	�
B	�B	�B	�	B	�B	�B	�B	�!B	�.B	�:B	�NB	�QB	�PB	�PB	�YB	�]B	�[B	�_B	�[B	�^B	�YB	�^B	�^B	�oB	�}B	�~B	�yB	�{B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
�B
�B
�B
�B
B
�B
 B
B
B
B
B
B
B
B
B
B
B
B
B
B

B
B
B
/B
-B
�B
%�B
-�B
2B
8;B
:DB
@iB
F�B
I�B
O�B
V�B
]B
c;B
gSB
koB
p�B
s�B
x�B
|�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.04 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =1(+/-0), vertically averaged dS =0.001(+/-0.001) in PSS-78.                                                                                                                                                                                                  Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Map scales: x=6,3; y=2,1. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                            201906040940002019060409400020190604094000  AO  ARCAADJP                                                                    20160224101612    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20160224101612  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20160224101612  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190604094000  IP                  G�O�G�O�G�O�                