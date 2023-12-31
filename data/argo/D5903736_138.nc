CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2016-02-03T20:16:40Z AOML 3.0 creation; 2016-05-31T19:14:47Z UW 3.1 conversion     
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
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �,   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �<   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �@   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �P   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �T   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �X   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �\Argo profile    3.1 1.2 19500101000000  20160203201640  20190604093959  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  4051_7090_138                   2C  D   APEX                            5368                            041511                          846 @ג��G`R1   @ג�xjD�@4��;dZ�d`bM��1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   A   A   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8ffB@ffBHffBP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dts3Dys3D�fD�FfD�l�D���D�3D�P D�s3D��fD��D�9�D���D���D�3D�0 DچfD��3D�  D�33D� D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��R@��RA\)A?\)A_\)A\)A��A��A��A��AϮA߮A�A��B�
B�
B�
B�
B'�
B/�
B8=pB@=pBH=pBO�
BW�
B_�
Bg�
Bo�
Bw�
B�
B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D }qD �qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD	}qD	�qD
}qD
�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD }qD �qD!}qD!�qD"}qD"�qD#}qD#�qD$}qD$�qD%}qD%�qD&}qD&�qD'}qD'�qD(}qD(�qD)}qD)�qD*}qD*�qD+}qD+�qD,}qD,�qD-}qD-�qD.}qD.�qD/}qD/�qD0}qD0�qD1}qD1�qD2}qD2�qD3}qD3�qD4}qD4�qD5}qD5�qD6}qD6�qD7}qD7�qD8}qD8�qD9}qD9�qD:}qD:�qD;}qD;�qD<}qD<�qD=}qD=�qD>}qD>�qD?}qD?�qD@}qD@�qDA}qDA�qDB}qDB�qDC}qDC�qDD}qDD�qDE}qDE�qDF}qDF�qDG}qDG�qDH}qDH�qDI}qDI�qDJ}qDJ�qDK}qDK�qDL}qDL�qDM}qDM�qDN}qDN�qDO}qDO�qDP}qDP�qDQ}qDQ�qDR}qDR�qDS}qDS�qDT}qDT�qDU}qDU�qDV}qDV�qDW}qDW�qDX}qDX�qDY}qDY�qDZ}qDZ�qD[}qD[�qD\}qD\�qD]}qD]�qD^}qD^�qD_}qD_�qD`}qD`�qDa}qDa�qDb}qDb�qDc}qDc�qDd}qDd�qDe}qDe�qDf}qDf�qDg}qDg�qDh}qDh�qDi}qDi�qDj}qDj�qDk}qDk�qDl}qDl�qDm}qDm�qDn}qDn�qDo}qDo�qDp}qDp�qDq}qDq�qDr}qDr�qDs}qDs�qDtp�Dyp�D�D�ED�k�D��RD��D�N�D�q�D��D��D�8RD���D�˅D��D�.�DڅD���D���D�1�D�~�D��R11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��A� �A� �A�"�A�(�A�+A�+A�+A�+A�+A�$�A��AʍPA���A��TA��/A�dZA�C�A�9XA�bA�XA���A§�A�E�A�A�A�+A�A���A��uA�jA�5?A��
A���A�dZA��yA�I�A�ȴA��^A��-A�Q�A���A�
=A��jA�7LA��A�%A��hA��uA�1'A�?}A��A��A�S�A�ȴA�^5A�33A�Q�A��A��^A�5?A��A��9A���A�jA�z�A��TA���A���A�/A�ĜA�?}A�  A���A�v�A�G�A��FA�1A�jA��A��/A�JA���A�5?A��yA���A�9XA�^5A���A�dZA��PA�|�A��
A�1'A���A�t�A��A�5?A~ZA}�A}G�A{��AzE�Ax�HAw�wAwAv�Au��At�Ar�RAr�Ar�Ar1AqC�Ap��An��Am+Al�Ak�Ai�AhVAg\)Af�jAe��Ab�A` �A^A�A]�AY%AU�TAS��AQ�APbNANZAL�`AI�7AF��AE?}AD^5AC7LAB�yAB��ABȴABĜAB��AB��AB��AB~�AB^5AA��A?dZA<��A:�HA9�A9�wA9p�A8�+A7/A4-A29XA1�A1oA0�A/��A/7LA.I�A-\)A+x�A)"�A'A&I�A$I�A#��A"�9A!/A r�A+A/AoA�AƨAp�A�A/A��A��A�+A�hAK�A��A��AdZA�HA(�A�A\)A�A~�A�hA�wA	�A	��A	�A	\)AbA�AJA"�A��AQ�A�7AoA�A ��@��@���@��@�|�@��R@��@��;@��H@�E�@���@�~�@��#@�@�`B@�%@�w@�7L@�C�@�O�@�\)@��@�A�@�\)@���@��@�@��@�&�@�bN@�A�@�E�@�I�@�33@ՙ�@���@���@ҟ�@�@��@θR@��#@���@̋D@���@ˍP@��H@�@�@��@��@���@Ų-@�X@��@Ĵ9@Ĵ9@ģ�@��@�"�@�p�@��9@�9X@��m@�ȴ@��+@���@�I�@���@���@���@�ff@���@�n�@��T@��h@�p�@�X@��@�r�@��@�S�@�n�@��^@�p�@�hs@�O�@�?}@�%@� �@��;@��@�ȴ@���@�-@��T@��-@�X@�%@��@�bN@��@�C�@�v�@�@��@��#@��#@�@�x�@���@���@�bN@�b@���@�33@���@��@��@�`B@�G�@�V@��D@�z�@�Z@�1@�ƨ@��@��P@�t�@�\)@�+@��H@��!@�n�@�-@��@�@��7@��7@�X@���@��9@��@��
@�C�@��H@�ff@���@��@��+@��\@�M�@��@��h@�7L@�?}@��@��/@�9X@�9X@�9X@��@�1@�1'@�Z@�bN@�A�@��;@���@��@�l�@�\)@�
=@��\@�^5@�-@���@���@���@��@�X@��@��/@���@��9@���@�z�@�j@�Q�@���@���@�~�@�ff@��+@���@���@�v�@��@�`B@���@��D@�1@��;@��F@�K�@��y@�ȴ@��!@���@���@�^5@��@��T@���@�`B@��@���@�Ĝ@���@��@�o@���@���@���@���@�ff@�E�@�-@���@���@�O�@�O�@�7L@��@��@�V@��`@�Ĝ@��j@��j@���@�I�@�b@��
@�l�@�"�@�"�@��@���@�~�@�5?@�x�@��`@�9X@��F@�o@�@��@�b@� �@��F@�\)@���@���@�=q@�@���@���@�`B@���@�Ĝ@��D@���@���@���@��@�&�@x�@r��@i��@`��@X1'@N��@G��@@�9@<z�@8b@1��@+�m@%�@ �u@�m@v�@-@�@	�711111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��A� �A� �A�"�A�(�A�+A�+A�+A�+A�+A�$�A��AʍPA���A��TA��/A�dZA�C�A�9XA�bA�XA���A§�A�E�A�A�A�+A�A���A��uA�jA�5?A��
A���A�dZA��yA�I�A�ȴA��^A��-A�Q�A���A�
=A��jA�7LA��A�%A��hA��uA�1'A�?}A��A��A�S�A�ȴA�^5A�33A�Q�A��A��^A�5?A��A��9A���A�jA�z�A��TA���A���A�/A�ĜA�?}A�  A���A�v�A�G�A��FA�1A�jA��A��/A�JA���A�5?A��yA���A�9XA�^5A���A�dZA��PA�|�A��
A�1'A���A�t�A��A�5?A~ZA}�A}G�A{��AzE�Ax�HAw�wAwAv�Au��At�Ar�RAr�Ar�Ar1AqC�Ap��An��Am+Al�Ak�Ai�AhVAg\)Af�jAe��Ab�A` �A^A�A]�AY%AU�TAS��AQ�APbNANZAL�`AI�7AF��AE?}AD^5AC7LAB�yAB��ABȴABĜAB��AB��AB��AB~�AB^5AA��A?dZA<��A:�HA9�A9�wA9p�A8�+A7/A4-A29XA1�A1oA0�A/��A/7LA.I�A-\)A+x�A)"�A'A&I�A$I�A#��A"�9A!/A r�A+A/AoA�AƨAp�A�A/A��A��A�+A�hAK�A��A��AdZA�HA(�A�A\)A�A~�A�hA�wA	�A	��A	�A	\)AbA�AJA"�A��AQ�A�7AoA�A ��@��@���@��@�|�@��R@��@��;@��H@�E�@���@�~�@��#@�@�`B@�%@�w@�7L@�C�@�O�@�\)@��@�A�@�\)@���@��@�@��@�&�@�bN@�A�@�E�@�I�@�33@ՙ�@���@���@ҟ�@�@��@θR@��#@���@̋D@���@ˍP@��H@�@�@��@��@���@Ų-@�X@��@Ĵ9@Ĵ9@ģ�@��@�"�@�p�@��9@�9X@��m@�ȴ@��+@���@�I�@���@���@���@�ff@���@�n�@��T@��h@�p�@�X@��@�r�@��@�S�@�n�@��^@�p�@�hs@�O�@�?}@�%@� �@��;@��@�ȴ@���@�-@��T@��-@�X@�%@��@�bN@��@�C�@�v�@�@��@��#@��#@�@�x�@���@���@�bN@�b@���@�33@���@��@��@�`B@�G�@�V@��D@�z�@�Z@�1@�ƨ@��@��P@�t�@�\)@�+@��H@��!@�n�@�-@��@�@��7@��7@�X@���@��9@��@��
@�C�@��H@�ff@���@��@��+@��\@�M�@��@��h@�7L@�?}@��@��/@�9X@�9X@�9X@��@�1@�1'@�Z@�bN@�A�@��;@���@��@�l�@�\)@�
=@��\@�^5@�-@���@���@���@��@�X@��@��/@���@��9@���@�z�@�j@�Q�@���@���@�~�@�ff@��+@���@���@�v�@��@�`B@���@��D@�1@��;@��F@�K�@��y@�ȴ@��!@���@���@�^5@��@��T@���@�`B@��@���@�Ĝ@���@��@�o@���@���@���@���@�ff@�E�@�-@���@���@�O�@�O�@�7L@��@��@�V@��`@�Ĝ@��j@��j@���@�I�@�b@��
@�l�@�"�@�"�@��@���@�~�@�5?@�x�@��`@�9X@��F@�o@�@��@�b@� �@��F@�\)@���@���@�=q@�@���@���@�`B@���@�Ĝ@��D@���@���@���@��@�&�@x�@r��@i��@`��@X1'@N��@G��@@�9@<z�@8b@1��@+�m@%�@ �u@�m@v�@-@�@	�711111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB-B,B,B,B-B-B-B-B-B-B.B/B=qB�JB�B�fB�B�B�B��B��B+B\B�B(�B'�B$�B)�B+B.B2-B49B8RB9XB9XB:^B>wBC�BC�BC�BB�B?}B33B�B�B{B\B1B%BB��BBB��B�B�`B�
BƨB�}B�RB��B��B�JB{�Bo�BgmBVBO�BK�BF�B?}B;dB33B!�B��B�B�ZB�B��BȴB��B�DBr�B`BBZBR�BI�B=qB.B"�B�BDBB
�B
��B
��B
�}B
�B
��B
��B
�hB
�B
y�B
r�B
iyB
n�B
k�B
`BB
T�B
P�B
O�B
N�B
I�B
C�B
49B
#�B
"�B
�B
DB	��B	�B	�B	�BB	�qB	��B	��B	�hB	t�B	dZB	R�B	C�B	8RB	,B	!�B	�B	JB	B��B	B	1B	
=B	
=B	DB	DB	DB	DB	
=B	1B	B��B�NB�B�B�B��B��BǮBǮBǮBƨBƨBƨBŢBÖB��B�qB�?B�B��B��B��B��B�hB�JB�1B�B� B{�B{�B{�B�B�1B�=B�JB�Bz�Bv�Bt�Br�Bs�Bt�Bv�Bv�Bx�Bx�Bx�Bx�Bv�Bu�Bs�Br�Bq�Bp�Bk�BhsBgmBffBe`BdZBbNB_;B]/B[#B]/B]/B\)B[#B[#B[#B]/B^5B^5B^5B`BB`BB`BB_;B^5B\)B[#BZB[#B\)B`BB`BBaHBcTBdZBffBgmBhsBiyBhsBk�Bo�Bo�Br�Bu�Bw�Bw�Bx�Bz�B|�B}�B�B�%B�7B�PB�uB��B��B��B��B��B��B��B��B��B�B�B�B�3B�^B�jB�}BBȴB��B�
B�B�B�#B�HB�fB�B�B�B��B��B��B��B��B��B��B	B	
=B	JB	JB	JB	PB	VB	{B	�B	�B	�B	�B	 �B	"�B	%�B	&�B	(�B	,B	-B	/B	1'B	33B	5?B	7LB	8RB	9XB	:^B	=qB	F�B	H�B	I�B	K�B	O�B	R�B	YB	_;B	cTB	e`B	e`B	gmB	k�B	k�B	k�B	n�B	p�B	r�B	s�B	t�B	u�B	v�B	x�B	z�B	|�B	}�B	� B	�B	�B	�%B	�+B	�+B	�+B	�+B	�%B	�B	�B	�7B	�PB	�bB	�uB	�{B	��B	��B	�{B	�uB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�'B	�-B	�-B	�-B	�-B	�3B	�?B	�?B	�FB	�RB	�XB	�^B	�^B	�dB	�dB	�jB	�jB	�qB	��B	B	ÖB	ÖB	ÖB	ÖB	ĜB	ŢB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�#B	�/B	�HB	�NB	�NB	�NB	�TB	�ZB	�mB	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
\B
�B
!�B
,B
5?B
;dB
>wB
C�B
F�B
J�B
P�B
YB
]/B
bNB
gmB
iyB
n�B
r�B
w�B
{�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B-�B,�B,�B,�B-�B-�B-�B-�B-�B-�B.�B/�B>GB�!B��B�;B�VB�yB�B��B��BB4B�B)�B(�B%�B*�B+�B.�B3B5B9.B:1B:1B;8B?NBDlBDlBDnBCiB@PB4B �BmBNB2B	B B�B��B�B�B��B�tB�5B��BǀB�[B�)B��B��B�B|�BprBhGBV�BP�BL�BG~B@QB<>B4B"�B��B�vB�3B��B��BɋB��B�Bs�BaBZ�BS�BJ�B>HB.�B#�BXBB�B
�YB
ϰB
˔B
�RB
��B
��B
��B
�;B
��B
z�B
s�B
jKB
okB
lWB
aB
U�B
Q�B
P�B
O�B
J�B
DfB
5B
$�B
#�B
sB
B	��B	�B	�_B	�B	�FB	��B	�dB	�:B	u�B	e,B	S�B	DgB	9%B	,�B	"�B	VB	B	�B��B	�B		B	B	B	B	B	B	B	B		B	�B��B�B��B��B��B��BϩB�|BȃB�B�wB�{B�yB�vB�dB�[B�>B�B��B��B��B�kB�TB�;B�B�B��B��B|�B|�B|�B��B�B�
B�B��B{�Bw�Bu�Bs~Bt�Bu�Bw�Bw�By�By�By�By�Bw�Bv�Bt�BsBryBqrBlTBiFBh;Bg4Bf.Be+Bc B`
B^B[�B]�B]�B\�B[�B[�B[�B]�B_B_B_BaBaBaB`B_B\�B[�BZ�B[�B\�BaBaBbBd!Be(Bg4Bh<BiFBjHBiCBlSBpkBppBsBv�Bx�Bx�By�B{�B}�B~�B��B��B�B�!B�AB�YB�`B�iB�hB�{B��B��B��B��B��B��B��B�B�-B�:B�MB�`BɇBѵB��B��B��B��B�B�6B�SB�sB�B��B��B��B��B��B��B��B	�B	B	B	B	B	B	&B	KB	]B	qB	�B	�B	!�B	#�B	&�B	'�B	)�B	,�B	-�B	/�B	1�B	4B	6B	8B	9"B	:+B	;2B	>CB	GyB	I�B	J�B	L�B	P�B	S�B	Y�B	`B	d#B	f2B	f3B	h@B	lYB	lVB	lXB	okB	qwB	s�B	t�B	u�B	v�B	w�B	y�B	{�B	}�B	~�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�1B	�FB	�MB	�VB	�RB	�KB	�HB	�EB	�KB	�TB	�pB	�xB	��B	��B	��B	��B	��B	��B	��B	��B	� B	��B	��B	�B	�B	�B	�B	�B	�$B	�)B	�0B	�/B	�7B	�6B	�:B	�=B	�BB	�TB	�_B	�hB	�kB	�gB	�jB	�pB	�rB	ȀB	ʍB	ˑB	̛B	͟B	ΡB	бB	ѶB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�"B	�'B	�.B	�>B	�QB	�`B	�oB	�~B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B	��B	��B	��B	��B	��B	��B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
/B
oB
"�B
,�B
6B
<5B
?HB
DlB
GvB
K�B
Q�B
Y�B
^ B
c B
h?B
jMB
ojB
s}B
x�B
|�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.04 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =1(+/-0), vertically averaged dS =0.001(+/-0.001) in PSS-78.                                                                                                                                                                                                  Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Map scales: x=6,3; y=2,1. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                            201906040939592019060409395920190604093959  AO  ARCAADJP                                                                    20160203201640    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20160203201640  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20160203201640  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190604093959  IP                  G�O�G�O�G�O�                