CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:37:49Z creation;2022-06-04T17:37:49Z conversion to V3.1      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  78   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7x   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8<   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8@   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8H   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8L   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8T   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8\   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8d   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8h   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8p   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8t   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8x   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8|   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9|   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I4   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M$   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p|   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tl   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �h   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �X   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �t   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ޤ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
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
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20220604173749  20220610131507  5905853                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               TA   JA                                  2B  A   APEX                            8421                            2.11.2                          846 @�wc�i�1   @�wd����@.�`A�7L�c��t�j1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�33B   B  B  B  B   B(  B0  B8  B@  BHffBPffBX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C L�C��C�fC�fC  C
  C  C  C  C  C  C  C  C  C�C�C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL�CN  CP  CR  CT  CV�CW�fCZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct�Cv� Cw��Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DHy�DI  DI� DJ  DJ�fDK  DK� DL  DL�fDM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn�fDofDo� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�6f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @
>@}p�@��R@��RA\)A?\)A_\)A\)A��A��A��A��AϮA߮A��GA��B�
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
B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B˸RBϸRB��B��B��B��B��B��B��B��B��B��B��C B�CC�)C�)C��C	��C��C��C��C��C��C��C��C��C]C]C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CL]CM��CO��CQ��CS��CV]CW�)CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Ct]Cvu�CwCy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���D }qD �qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD	}qD	�qD
}qD
�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD }qD �qD!}qD!�qD"}qD"�qD#}qD#�qD$}qD$�qD%}qD%�qD&}qD&�qD'}qD'�qD(}qD(�qD)}qD)�qD*}qD*�qD+}qD+�qD,}qD,�qD-}qD-�qD.}qD.�qD/}qD/�qD0}qD0�qD1}qD1�qD2}qD2�qD3}qD3�qD4}qD4�qD5}qD5�qD6}qD6�qD7}qD7�qD8}qD8�qD9}qD9�qD:}qD:�qD;}qD;�qD<}qD<�qD=}qD=�qD>}qD>�qD?}qD?�qD@}qD@�qDA}qDA�qDB}qDB�qDC}qDC�qDD}qDD�qDE}qDE�qDF}qDF�qDG}qDG�qDHwDH�qDI}qDI�qDJ��DJ�qDK}qDK�qDL��DL�qDM}qDM�qDN}qDN�qDO}qDO�qDP}qDP�qDQ}qDQ�qDR}qDR�qDS}qDS�qDT}qDT�qDU}qDU�qDV}qDV�qDW}qDW�qDX}qDX�qDY}qDY�qDZ}qDZ�qD[}qD[�qD\}qD\�qD]}qD]�qD^}qD^�qD_}qD_�qD`}qD`�qDa}qDa�qDb}qDb�qDc}qDc�qDd}qDd�qDe}qDe�qDf}qDf�qDg}qDg�qDh}qDh�qDi}qDi�qDj}qDj�qDk}qDk�qDl}qDl�qDm}qDm�qDn��Do�Do}qDo�qDp}qDp�qDq}qDq�qDr}qDr�qDs}qDs�qDt}qDt�qDu}qDu�qDv}qDv�qDw}qDw�qDx}qDx�qDy}qDy�qDz}qDz�qD{}qD{�qD|}qD|�qD}}qD}�qD~}qD~�qD}qD�qD�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D��D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D¾�D���D�>�D�~�Dþ�D���D�>�D�~�Dľ�D���D�>�D�~�Dž�D���D�>�D�~�Dƾ�D���D�>�D�~�DǾ�D���D�>�D�~�DȾ�D���D�>�D�~�Dɾ�D���D�>�D�~�Dʾ�D���D�>�D�~�D˾�D���D�>�D�~�D̾�D���D�>�D�~�D;�D���D�>�D�~�Dξ�D���D�>�D�~�DϾ�D���D�>�D�~�Dо�D���D�>�D�~�DѾ�D���D�>�D�~�DҾ�D���D�>�D�~�DӾ�D���D�>�D�~�DԾ�D���D�>�D�~�Dվ�D���D�>�D�~�D־�D���D�>�D�~�D׾�D���D�>�D�~�Dؾ�D���D�>�D�~�Dپ�D���D�>�D�~�Dھ�D���D�>�D�~�D۾�D���D�>�D�~�Dܾ�D���D�>�D�~�Dݾ�D���D�>�D�~�D޾�D���D�>�D�~�D߾�D���D�>�D�~�DྸD���D�>�D�~�DᾸD���D�>�D�~�D⾸D���D�>�D�~�D㾸D���D�>�D�~�D侸D���D�>�D�~�D徸D���D�>�D�~�D澸D���D�>�D�~�D羸D���D�>�D�~�D辸D���D�>�D�~�D龸D���D�>�D�~�D꾸D���D�>�D�~�D뾸D���D�>�D�~�D쾸D���D�>�D�~�D���D���D�>�D�~�DD���D�>�D�~�DﾸD���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�5111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A���A�ʌA��)A���A���A��<A��A��0A��6A���A��aA��?A�ٴA���A�خA��A��#A��A��A��WA��)A��HA���A���A��A̺*A̰�Ã�A�}�A�v�A�g�A�c�A�^A�S�A�@�A�CA˒A��UA��A�7LA���A�aA�O�A�B[A�"�A��AA�h
AėYA�:�A�˒AÛ	A��A��A�ÖA�A�A�:�A�}�A�;A���A���A�S�A���A�\�A�.A���A��A��hA��A��A��wA��
A���A�]dA��1A�$@A�רA���A��A�ffA�-CA���A�ܒA��TA�ZA�D3A�%�A��A�@A�=�A��A�D�A��GA�v�A���A���A�ÖA���A�u�A���A��6A�[WA�@A�FA�s�A�o A���A�?HA�Az�mAtoAoAlRTAj�Ai�'Ah��AeH�Aa�KA]��AX�4AQ�}AM�AJ�vAH֡AC�jABGEAA��A@�A=��A:�A:tTA9�QA9qvA8�KA7��A58�A3�PA3�A1�\A0S&A0uA0H�A0��A.�;A,�A+��A+-�A*z�A*|A)iDA'�*A&�A%�A%c�A$��A$��A%��A%�sA$Z�A$"�A# �A!�A!FtA ��A F�Av�A� AAXyA�A4�Ac A�+A�!A+A�jA�*AXA��A��A�zAa�A�^A��A�AAv�A�+A/�A�A�A��AOA{AɆA%A��A��A��AkQA�APHACA�A�	A-wA�AVA�;A��AuA=�A��AARTAeA:�A
��A	qAA�KA�"AQAA�Al�A�oA9�A��Ag8A��A�AGAJ#A�KA�zA�A*0A �mA U2A ]�A ��A ��A ��A �3A g8@�PH@�1�@�{@�
=@���@�*0@��@�rG@�_�@��h@�-�@�6@���@@��s@�7@�4@��+@�~�@���@���@�"h@�f@�S@�h
@��o@��Q@��@���@�Q�@��m@�Ft@��5@��@�!-@�֡@���@��8@�($@ۇ�@�f�@�B�@�+@�҉@�bN@�o @ؽ<@�C�@���@�s@�;@֜x@��@�;d@ԭ�@�ԕ@�Z�@���@Ҍ�@�@�zx@�!-@Ф�@�!�@���@ϭC@�%F@�bN@ͼ�@�t�@�4@̬@̃@��]@��s@�Ta@ɸ�@�;d@�Ɇ@�v�@�	@���@���@�c@�N<@���@��p@Ɗr@�3�@��@��m@ŷ�@Ş�@�Y�@���@��,@��@ě�@�'R@�;d@��@�Z�@��v@��.@��@�J�@�!�@���@��~@��-@���@�6z@�ߤ@�\�@��@���@�E�@��@��@��{@�W?@�@���@��@���@���@���@���@�f�@�*0@���@��1@���@�^�@�(@��'@�@��w@�+�@��,@��Y@�8�@�M�@�x@�s�@��r@�0U@��T@���@��9@��N@���@���@�+@�h
@�� @�rG@� i@���@���@�i�@�A�@�#:@��@���@�4�@�y>@�!@��g@���@�A @��@��@��5@��@�-@���@�u�@�B�@��9@�J�@�b@���@���@�B�@���@���@�ff@�$@��j@�qv@��@��@�v�@�q�@�Q@�"h@��$@��@�YK@���@���@���@���@�4@���@��@�b�@�I�@���@�bN@�Ft@�K^@�e�@��F@���@��P@�\�@�a�@�o @�e�@���@�D�@�*�@��@���@�\)@�4�@���@�>B@�s@���@���@�J�@���@��D@� �@���@��n@���@�~�@�j@�A @��K@��b@�8�@��@��h@�g�@�/�@��@�ں@���@�r�@�/�@���@���@���@��M@��4@�c@�;d@��1@�g8@�I�@�(�@��@��@���@��@@�g�@��P@��s@�~(@��@��>@��@�iD@�L�@�-w@�@@���@��@���@��b@��@��Y@�3�@��@���@��@�zx@�k�@�U�@��@���@�E�@�1@���@��}@��{@�e,@�S&@��@���@�9X@���@���@���@���@�=�@��"@���@�_@�e@���@��	@�Dg@��@�ی@���@�H�@�O@��@���@�>�@�"�@��@���@��.@�c @�H�@�'R@��@�P@)_@~��@~�6@~q�@~L0@}�z@}Dg@}#�@|��@|2�@{�[@{E9@z�R@y�-@y^�@x�@x�j@x�@w��@w��@w6z@v�}@va|@u}�@u?}@t�5@t�U@tK^@s�r@sZ�@r��@rff@r@q�@q��@p��@p��@p�@o�;@ot�@o�@n��@np;@n($@m��@m�@l��@k��@k;d@j�@j�A@j@�@i�'@h��@g� @g�4@g1�@fC�@e�9@eVm@d��@d_@c��@cU�@b�b@a�D@a�~@a@`�.@`[�@`G@_�P@_K�@_F�@_C@^��@]�o@]��@]hs@]#�@\�|@\��@\(�@[X�@Z҉@Z}V@Z$�@Y��@Yo @Y5�@YV@XĜ@X��@XPH@X"h@W�@W]�@W9�@V��@V�h@VZ�@U�>@U�X@U \@T��@T�@T�@T9X@Sl�@Rں@R� @RZ�@Q�@Qa�@P�/@P��@Pw�@P/�@P~@O��@O��@O�f@O@O@N��@N0U@M��@Mu�@M0�@L�`@LD�@K�
@K�V@Ke�@K!-@J�'@J�b@J�+@J{�@I��@I��@I�S@IX@I(�@I+@H��@HH@G�m@G{J@G9�@G�@G�@G i@F�6@F1�@E��@E�9@E��@Em]@EB�@D�5@D��@D��@DA�@C~�@Ca@C(@Bȴ@B��@B\�@B	@A�@ArG@AA @@�[@@C-@?��@?RT@?�@>\�@=�Z@=��@=^�@=�@<֡@<�j@<��@<oi@<�@;�;@;�{@;>�@;@:�x@:YK@9�@9�t@9a�@9�@8��@8D�@7�@7s@7@6�L@6GE@64@5�@5w2@52a@4ѷ@4`�@41'@4M@3�k@3C@2�h@2d�@2H�@2�@1�)@1��@1f�@1\�@1S&@1�@0��@0Z@0�@/��@/��@/��@/��@/S�@/(@.�@.͟@.�6@.i�@.Q@.�@-�#@-�z@-�-@-k�@,��@,�?@,ѷ@,�u@,oi@,S�@,7�@+خ@+��@+o�@*��@*�R@*��@*\�@*?@*�@*_@)��@)��@)�h@)hs@(�@(�?@(�j@(��@(Z@(7�@(*�@(b@'��@'"�@'(@&�X@&�r@&h
@&;�@&�@%�@%G�@$��@$M@$%�@#��@#|�@"�M@"��@"�A@"�b@"��@"�r@"1�@!�N@!��@!�@!J�@ �	@ ��@ PH@�r@�@��@S�@C�@��@�@q�@)�@��@��@��@S�@��@s@O@�@�m@�r@ff@.�@e@!�@.�@($@$�@�@�'@�@N�@H@ �@�@��@S�@
=@��@�\@{�@p;@Z�@.�@@�D@�@�X@��@u�@8�@-w@%F@�@�`@��@[�@M@D�@2�@�+@�a@��@y�@S�@/�@"�@o@��@��@~�@n�@d�@^5@Ta@:*@
�@�@��@�^@�C@�@�~@rG@IR@+�@�@�5@�@�@�U@Ĝ@��@m�@e�@bN@N�@I�@,=@�@�g@��@�}@��@n/@E9@�@�!@s�@L0@	@u@��@�M@^�@q@��@�5@�@��@�$@��@��@��@r�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A���A�ʌA��)A���A���A��<A��A��0A��6A���A��aA��?A�ٴA���A�خA��A��#A��A��A��WA��)A��HA���A���A��A̺*A̰�Ã�A�}�A�v�A�g�A�c�A�^A�S�A�@�A�CA˒A��UA��A�7LA���A�aA�O�A�B[A�"�A��AA�h
AėYA�:�A�˒AÛ	A��A��A�ÖA�A�A�:�A�}�A�;A���A���A�S�A���A�\�A�.A���A��A��hA��A��A��wA��
A���A�]dA��1A�$@A�רA���A��A�ffA�-CA���A�ܒA��TA�ZA�D3A�%�A��A�@A�=�A��A�D�A��GA�v�A���A���A�ÖA���A�u�A���A��6A�[WA�@A�FA�s�A�o A���A�?HA�Az�mAtoAoAlRTAj�Ai�'Ah��AeH�Aa�KA]��AX�4AQ�}AM�AJ�vAH֡AC�jABGEAA��A@�A=��A:�A:tTA9�QA9qvA8�KA7��A58�A3�PA3�A1�\A0S&A0uA0H�A0��A.�;A,�A+��A+-�A*z�A*|A)iDA'�*A&�A%�A%c�A$��A$��A%��A%�sA$Z�A$"�A# �A!�A!FtA ��A F�Av�A� AAXyA�A4�Ac A�+A�!A+A�jA�*AXA��A��A�zAa�A�^A��A�AAv�A�+A/�A�A�A��AOA{AɆA%A��A��A��AkQA�APHACA�A�	A-wA�AVA�;A��AuA=�A��AARTAeA:�A
��A	qAA�KA�"AQAA�Al�A�oA9�A��Ag8A��A�AGAJ#A�KA�zA�A*0A �mA U2A ]�A ��A ��A ��A �3A g8@�PH@�1�@�{@�
=@���@�*0@��@�rG@�_�@��h@�-�@�6@���@@��s@�7@�4@��+@�~�@���@���@�"h@�f@�S@�h
@��o@��Q@��@���@�Q�@��m@�Ft@��5@��@�!-@�֡@���@��8@�($@ۇ�@�f�@�B�@�+@�҉@�bN@�o @ؽ<@�C�@���@�s@�;@֜x@��@�;d@ԭ�@�ԕ@�Z�@���@Ҍ�@�@�zx@�!-@Ф�@�!�@���@ϭC@�%F@�bN@ͼ�@�t�@�4@̬@̃@��]@��s@�Ta@ɸ�@�;d@�Ɇ@�v�@�	@���@���@�c@�N<@���@��p@Ɗr@�3�@��@��m@ŷ�@Ş�@�Y�@���@��,@��@ě�@�'R@�;d@��@�Z�@��v@��.@��@�J�@�!�@���@��~@��-@���@�6z@�ߤ@�\�@��@���@�E�@��@��@��{@�W?@�@���@��@���@���@���@���@�f�@�*0@���@��1@���@�^�@�(@��'@�@��w@�+�@��,@��Y@�8�@�M�@�x@�s�@��r@�0U@��T@���@��9@��N@���@���@�+@�h
@�� @�rG@� i@���@���@�i�@�A�@�#:@��@���@�4�@�y>@�!@��g@���@�A @��@��@��5@��@�-@���@�u�@�B�@��9@�J�@�b@���@���@�B�@���@���@�ff@�$@��j@�qv@��@��@�v�@�q�@�Q@�"h@��$@��@�YK@���@���@���@���@�4@���@��@�b�@�I�@���@�bN@�Ft@�K^@�e�@��F@���@��P@�\�@�a�@�o @�e�@���@�D�@�*�@��@���@�\)@�4�@���@�>B@�s@���@���@�J�@���@��D@� �@���@��n@���@�~�@�j@�A @��K@��b@�8�@��@��h@�g�@�/�@��@�ں@���@�r�@�/�@���@���@���@��M@��4@�c@�;d@��1@�g8@�I�@�(�@��@��@���@��@@�g�@��P@��s@�~(@��@��>@��@�iD@�L�@�-w@�@@���@��@���@��b@��@��Y@�3�@��@���@��@�zx@�k�@�U�@��@���@�E�@�1@���@��}@��{@�e,@�S&@��@���@�9X@���@���@���@���@�=�@��"@���@�_@�e@���@��	@�Dg@��@�ی@���@�H�@�O@��@���@�>�@�"�@��@���@��.@�c @�H�@�'R@��@�P@)_@~��@~�6@~q�@~L0@}�z@}Dg@}#�@|��@|2�@{�[@{E9@z�R@y�-@y^�@x�@x�j@x�@w��@w��@w6z@v�}@va|@u}�@u?}@t�5@t�U@tK^@s�r@sZ�@r��@rff@r@q�@q��@p��@p��@p�@o�;@ot�@o�@n��@np;@n($@m��@m�@l��@k��@k;d@j�@j�A@j@�@i�'@h��@g� @g�4@g1�@fC�@e�9@eVm@d��@d_@c��@cU�@b�b@a�D@a�~@a@`�.@`[�@`G@_�P@_K�@_F�@_C@^��@]�o@]��@]hs@]#�@\�|@\��@\(�@[X�@Z҉@Z}V@Z$�@Y��@Yo @Y5�@YV@XĜ@X��@XPH@X"h@W�@W]�@W9�@V��@V�h@VZ�@U�>@U�X@U \@T��@T�@T�@T9X@Sl�@Rں@R� @RZ�@Q�@Qa�@P�/@P��@Pw�@P/�@P~@O��@O��@O�f@O@O@N��@N0U@M��@Mu�@M0�@L�`@LD�@K�
@K�V@Ke�@K!-@J�'@J�b@J�+@J{�@I��@I��@I�S@IX@I(�@I+@H��@HH@G�m@G{J@G9�@G�@G�@G i@F�6@F1�@E��@E�9@E��@Em]@EB�@D�5@D��@D��@DA�@C~�@Ca@C(@Bȴ@B��@B\�@B	@A�@ArG@AA @@�[@@C-@?��@?RT@?�@>\�@=�Z@=��@=^�@=�@<֡@<�j@<��@<oi@<�@;�;@;�{@;>�@;@:�x@:YK@9�@9�t@9a�@9�@8��@8D�@7�@7s@7@6�L@6GE@64@5�@5w2@52a@4ѷ@4`�@41'@4M@3�k@3C@2�h@2d�@2H�@2�@1�)@1��@1f�@1\�@1S&@1�@0��@0Z@0�@/��@/��@/��@/��@/S�@/(@.�@.͟@.�6@.i�@.Q@.�@-�#@-�z@-�-@-k�@,��@,�?@,ѷ@,�u@,oi@,S�@,7�@+خ@+��@+o�@*��@*�R@*��@*\�@*?@*�@*_@)��@)��@)�h@)hs@(�@(�?@(�j@(��@(Z@(7�@(*�@(b@'��@'"�@'(@&�X@&�r@&h
@&;�@&�@%�@%G�@$��@$M@$%�@#��@#|�@"�M@"��@"�A@"�b@"��@"�r@"1�@!�N@!��@!�@!J�@ �	@ ��@ PH@�r@�@��@S�@C�@��@�@q�@)�@��@��@��@S�@��@s@O@�@�m@�r@ff@.�@e@!�@.�@($@$�@�@�'@�@N�@H@ �@�@��@S�@
=@��@�\@{�@p;@Z�@.�@@�D@�@�X@��@u�@8�@-w@%F@�@�`@��@[�@M@D�@2�@�+@�a@��@y�@S�@/�@"�@o@��@��@~�@n�@d�@^5@Ta@:*@
�@�@��@�^@�C@�@�~@rG@IR@+�@�@�5@�@�@�U@Ĝ@��@m�@e�@bN@N�@I�@,=@�@�g@��@�}@��@n/@E9@�@�!@s�@L0@	@u@��@�M@^�@q@��@�5@�@��@�$@��@��@��@r�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B
�B
�B
�B
�B
�B
�B
�B
B
1B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
1B
�B
�B
�B
�B
�B
_B
B
�B
�B
�B
�B
�B
�B
�B
<B
�B
>�B
lB
r�B
}�B
~�B
�B
�[B
�YB
��B
�~B
� B
�B
�QB
��B
�_B
�sB
�B
��B
�RB
�+B�B6`BB[BCGBRoB`�BhXB�+B�0B��B��B�nB��B��B��B��B�_B��B�1B��B��B�B��B�_B�aBҽBөBϫB�rBB�lB�GB��B��B��B�rB}"Bl�BH�B72B(�B#B�B
�LB
�{B
�9B
�=B
�	B
k�B
F%B
�B	��B	�B	��B	��B	��B	��B	�pB	~wB	e`B	L�B	/OB	�B	�B��B��B�TB�-B�'B�B�B	�B	�B	uB	�B	B	'�B	>�B	EB	FB	F�B	R�B	\)B	k6B	vzB	s�B	`BB	f�B	wB	xB	�%B	� B	�B	��B	�B	�B	�B	�+B	��B	��B	��B	��B	��B	�RB	�(B	�YB	ĶB	��B	��B	�qB	��B	�xB	�6B	��B	��B	��B	�~B	ЗB	�kB	�NB	��B	�}B
$B
�B
�B
vB
KB
 �B	�6B
/B
�B
1B
�B
�B
�B	��B	�TB	�bB	�eB	�B	�B
�B
JB
pB
B
FB
�B
�B
�B
�B
�B
2B
CB
dB
"B
&�B
 �B
jB
!-B
gB
�B
.B
dB
	7B
�B
B
'B	�B	��B	�XB	��B	�'B	�CB	�;B	�RB	��B	�`B	��B	��B	�aB	��B	�B	�XB
B
	B
�B
�B
 iB	�B	�B	�B	�B	�OB	��B	�B	��B	�>B	�B	�`B	��B	�4B	�HB	��B	�B	�4B	��B	�\B	�_B	�MB	�B	�:B	҉B	�B	ӏB	��B	�gB	��B	�,B	�_B	��B	�B	�sB	��B	�B	�WB	��B	��B	��B	�=B	�B	�/B	�B	ݘB	�;B	ߊB	�'B	��B	��B	�vB	�'B	ߊB	��B	�B	�B	�hB	��B	�NB	��B	�B	��B	�B	�B	�@B	�@B	��B	�B	��B	��B	�FB	��B	�zB	��B	�B	��B	�`B	��B	�LB	�B	�B	�
B	�>B	�sB	��B	�DB	�B	��B	��B	�IB	��B	��B	��B	�B	�B	��B	�LB	�fB	�>B	�lB	��B	�XB	�	B	��B	�zB	�TB	�MB	�B	��B	�$B	��B	�>B	�>B	�lB	��B	�B	��B	�aB	�B	�B	�B	�[B	�-B	�B	��B	�+B	�8B	��B	�JB	�B	��B	��B	��B	�B	�B	�DB	��B	�>B	�8B	�2B	��B	�B	�B	��B	��B	�B	�9B	�B	�tB	��B	�ZB	�?B	�ZB	�?B	�B	�9B	��B	�?B	�tB	��B	�B	�zB	��B	�B	�fB	�RB	�B	�lB	�8B	�B	��B	��B	��B	��B	��B	�lB	��B	�	B	�^B	�JB	��B	��B	�<B	��B	�BB
 �B
'B
B
B
�B
�B
�B
�B
�B
B
�B
gB
B
{B
�B
'B
 �B	�B	�HB	��B	�<B	��B	�nB	�B	�B	� B	�B	�B	��B	�ZB	��B	��B
 B
�B
9B
�B
�B
�B
�B
BB
HB
 B
�B
�B
B
�B
�B
�B
�B
�B
�B
�B
�B
	B
=B
=B
WB
qB
�B
CB
xB
�B
xB
�B
dB
dB
�B
�B
jB
�B
�B
VB
!B
VB
�B
 �B
!bB
!�B
!HB
!�B
!-B
!bB
!�B
!�B
!�B
"hB
"4B
"�B
#:B
#TB
$B
#nB
$&B
$�B
$&B
#�B
$&B
$@B
$tB
%B
%FB
%�B
%�B
&fB
&LB
&�B
&2B
&�B
&�B
'�B
(
B
(�B
(
B
(�B
)*B
)*B
)B
)DB
)�B
)�B
*B
*B
*B
*0B
*�B
+B
+B
+�B
,=B
-B
-]B
-�B
-�B
./B
.IB
.�B
.�B
/ B
.�B
0B
/�B
/�B
0oB
0�B
0�B
1B
0�B
1'B
1AB
1�B
1�B
1�B
1�B
2aB
3B
3hB
3B
3B
3�B
4B
49B
4�B
5%B
5tB
5tB
5�B
6zB
6�B
72B
6�B
6�B
7�B
8�B
8�B
9�B
8�B
9rB
9�B
:^B
9�B
:*B
:B
:�B
:xB
;B
;�B
;�B
;�B
<�B
<�B
=<B
=�B
=qB
>B
>BB
>�B
?�B
@ B
?�B
@OB
AB
@�B
A;B
BAB
BuB
BuB
B�B
B�B
C�B
C�B
C�B
D�B
D�B
EB
E9B
E�B
E�B
FYB
F?B
F�B
GB
GEB
GEB
G_B
GzB
G�B
G�B
HB
HfB
H1B
H1B
H�B
IB
IB
IlB
IRB
IlB
I�B
I�B
I�B
J�B
J#B
JXB
JXB
J�B
KxB
KB
J�B
KxB
K�B
K�B
K�B
L0B
LJB
LdB
L0B
LdB
MB
M�B
M�B
NVB
N�B
OB
OB
N�B
N�B
OvB
N�B
OvB
OBB
O�B
PbB
P.B
Q B
Q B
QhB
Q4B
QB
Q�B
Q�B
Q�B
R B
R B
RB
R�B
R�B
R�B
S[B
S[B
S[B
SuB
S[B
S&B
TB
T�B
T{B
T�B
T�B
T�B
T�B
TaB
T�B
U2B
UB
U2B
UMB
U�B
VmB
V9B
V�B
V�B
X+B
X�B
XyB
XyB
YB
X�B
Y1B
YB
Y�B
Y�B
ZB
Z7B
Z�B
[#B
[WB
[=B
\CB
\]B
\�B
\�B
]B
]/B
]B
]dB
]/B
]dB
]~B
]�B
]�B
^B
^OB
^�B
^�B
^�B
_;B
_pB
_�B
_�B
`'B
`vB
`�B
a-B
a|B
a�B
a�B
a�B
bB
b�B
b�B
cB
b�B
c:B
c�B
c�B
c�B
c�B
dB
dZB
dZB
d�B
dZB
dtB
d�B
d�B
d�B
d�B
d�B
eB
e,B
e,B
e`B
ezB
ezB
ezB
e�B
e�B
fB
fB
f�B
ffB
ffB
f�B
gmB
g�B
h�B
h�B
h�B
iB
iyB
i�B
i�B
i�B
j0B
i�B
j0B
j�B
j�B
j�B
kB
kkB
kQB
k�B
k�B
l=B
k�B
l=B
lWB
l�B
lqB
mB
l�B
m]B
m]B
mwB
nB
n/B
m�B
n}B
n/B
ncB
n}B
nIB
ncB
nB
ncB
ncB
nB
m�B
m�B
n}B
pB
poB
p;B
p;B
p�B
p�B
qB
qB
q'B
p�B
qB
p�B
p�B
q[B
q�B
r�B
r�B
r�B
s�B
tB
t9B
tB
s�B
tTB
tB
tB
tB
tnB
t�B
t�B
t�B
uB
t�B
t�B
t�B
t�B
u%B
u�B
v�B
v�B
vFB
vzB
v�B
v�B
w2B
wLB
w�B
w�B
xB
xB
xRB
xlB
x�B
x�B
x�B
yrB
yrB
yrB
y�B
y�B
y�B
y�B
y�B
z^B
z�B
z�B
z�B
z�B
{dB
{�B
{�B
{B
{�B
{B
{�B
{�B
{�B
{�B
{�B
{�B
|B
{�B
|B
|B
|jB
|jB
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
}"B
}"B
|�B
}"B
}"B
}<B
}VB
}VB
}VB
}qB
}<B
}�B
}�B
~B
}�B
~B
~�B
~�B
~�B
.B
}B
�B
�B
�B
�B
��B
��B
��B
� B
� B
�UB
�;B
�UB
�oB
�oB
�oB
��B
��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B
�B
�B
�B
�B
�B
�B
�B
B
1B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
1B
�B
�B
�B
�B
�B
_B
B
�B
�B
�B
�B
�B
�B
�B
<B
�B
>�B
lB
r�B
}�B
~�B
�B
�[B
�YB
��B
�~B
� B
�B
�QB
��B
�_B
�sB
�B
��B
�RB
�+B�B6`BB[BCGBRoB`�BhXB�+B�0B��B��B�nB��B��B��B��B�_B��B�1B��B��B�B��B�_B�aBҽBөBϫB�rBB�lB�GB��B��B��B�rB}"Bl�BH�B72B(�B#B�B
�LB
�{B
�9B
�=B
�	B
k�B
F%B
�B	��B	�B	��B	��B	��B	��B	�pB	~wB	e`B	L�B	/OB	�B	�B��B��B�TB�-B�'B�B�B	�B	�B	uB	�B	B	'�B	>�B	EB	FB	F�B	R�B	\)B	k6B	vzB	s�B	`BB	f�B	wB	xB	�%B	� B	�B	��B	�B	�B	�B	�+B	��B	��B	��B	��B	��B	�RB	�(B	�YB	ĶB	��B	��B	�qB	��B	�xB	�6B	��B	��B	��B	�~B	ЗB	�kB	�NB	��B	�}B
$B
�B
�B
vB
KB
 �B	�6B
/B
�B
1B
�B
�B
�B	��B	�TB	�bB	�eB	�B	�B
�B
JB
pB
B
FB
�B
�B
�B
�B
�B
2B
CB
dB
"B
&�B
 �B
jB
!-B
gB
�B
.B
dB
	7B
�B
B
'B	�B	��B	�XB	��B	�'B	�CB	�;B	�RB	��B	�`B	��B	��B	�aB	��B	�B	�XB
B
	B
�B
�B
 iB	�B	�B	�B	�B	�OB	��B	�B	��B	�>B	�B	�`B	��B	�4B	�HB	��B	�B	�4B	��B	�\B	�_B	�MB	�B	�:B	҉B	�B	ӏB	��B	�gB	��B	�,B	�_B	��B	�B	�sB	��B	�B	�WB	��B	��B	��B	�=B	�B	�/B	�B	ݘB	�;B	ߊB	�'B	��B	��B	�vB	�'B	ߊB	��B	�B	�B	�hB	��B	�NB	��B	�B	��B	�B	�B	�@B	�@B	��B	�B	��B	��B	�FB	��B	�zB	��B	�B	��B	�`B	��B	�LB	�B	�B	�
B	�>B	�sB	��B	�DB	�B	��B	��B	�IB	��B	��B	��B	�B	�B	��B	�LB	�fB	�>B	�lB	��B	�XB	�	B	��B	�zB	�TB	�MB	�B	��B	�$B	��B	�>B	�>B	�lB	��B	�B	��B	�aB	�B	�B	�B	�[B	�-B	�B	��B	�+B	�8B	��B	�JB	�B	��B	��B	��B	�B	�B	�DB	��B	�>B	�8B	�2B	��B	�B	�B	��B	��B	�B	�9B	�B	�tB	��B	�ZB	�?B	�ZB	�?B	�B	�9B	��B	�?B	�tB	��B	�B	�zB	��B	�B	�fB	�RB	�B	�lB	�8B	�B	��B	��B	��B	��B	��B	�lB	��B	�	B	�^B	�JB	��B	��B	�<B	��B	�BB
 �B
'B
B
B
�B
�B
�B
�B
�B
B
�B
gB
B
{B
�B
'B
 �B	�B	�HB	��B	�<B	��B	�nB	�B	�B	� B	�B	�B	��B	�ZB	��B	��B
 B
�B
9B
�B
�B
�B
�B
BB
HB
 B
�B
�B
B
�B
�B
�B
�B
�B
�B
�B
�B
	B
=B
=B
WB
qB
�B
CB
xB
�B
xB
�B
dB
dB
�B
�B
jB
�B
�B
VB
!B
VB
�B
 �B
!bB
!�B
!HB
!�B
!-B
!bB
!�B
!�B
!�B
"hB
"4B
"�B
#:B
#TB
$B
#nB
$&B
$�B
$&B
#�B
$&B
$@B
$tB
%B
%FB
%�B
%�B
&fB
&LB
&�B
&2B
&�B
&�B
'�B
(
B
(�B
(
B
(�B
)*B
)*B
)B
)DB
)�B
)�B
*B
*B
*B
*0B
*�B
+B
+B
+�B
,=B
-B
-]B
-�B
-�B
./B
.IB
.�B
.�B
/ B
.�B
0B
/�B
/�B
0oB
0�B
0�B
1B
0�B
1'B
1AB
1�B
1�B
1�B
1�B
2aB
3B
3hB
3B
3B
3�B
4B
49B
4�B
5%B
5tB
5tB
5�B
6zB
6�B
72B
6�B
6�B
7�B
8�B
8�B
9�B
8�B
9rB
9�B
:^B
9�B
:*B
:B
:�B
:xB
;B
;�B
;�B
;�B
<�B
<�B
=<B
=�B
=qB
>B
>BB
>�B
?�B
@ B
?�B
@OB
AB
@�B
A;B
BAB
BuB
BuB
B�B
B�B
C�B
C�B
C�B
D�B
D�B
EB
E9B
E�B
E�B
FYB
F?B
F�B
GB
GEB
GEB
G_B
GzB
G�B
G�B
HB
HfB
H1B
H1B
H�B
IB
IB
IlB
IRB
IlB
I�B
I�B
I�B
J�B
J#B
JXB
JXB
J�B
KxB
KB
J�B
KxB
K�B
K�B
K�B
L0B
LJB
LdB
L0B
LdB
MB
M�B
M�B
NVB
N�B
OB
OB
N�B
N�B
OvB
N�B
OvB
OBB
O�B
PbB
P.B
Q B
Q B
QhB
Q4B
QB
Q�B
Q�B
Q�B
R B
R B
RB
R�B
R�B
R�B
S[B
S[B
S[B
SuB
S[B
S&B
TB
T�B
T{B
T�B
T�B
T�B
T�B
TaB
T�B
U2B
UB
U2B
UMB
U�B
VmB
V9B
V�B
V�B
X+B
X�B
XyB
XyB
YB
X�B
Y1B
YB
Y�B
Y�B
ZB
Z7B
Z�B
[#B
[WB
[=B
\CB
\]B
\�B
\�B
]B
]/B
]B
]dB
]/B
]dB
]~B
]�B
]�B
^B
^OB
^�B
^�B
^�B
_;B
_pB
_�B
_�B
`'B
`vB
`�B
a-B
a|B
a�B
a�B
a�B
bB
b�B
b�B
cB
b�B
c:B
c�B
c�B
c�B
c�B
dB
dZB
dZB
d�B
dZB
dtB
d�B
d�B
d�B
d�B
d�B
eB
e,B
e,B
e`B
ezB
ezB
ezB
e�B
e�B
fB
fB
f�B
ffB
ffB
f�B
gmB
g�B
h�B
h�B
h�B
iB
iyB
i�B
i�B
i�B
j0B
i�B
j0B
j�B
j�B
j�B
kB
kkB
kQB
k�B
k�B
l=B
k�B
l=B
lWB
l�B
lqB
mB
l�B
m]B
m]B
mwB
nB
n/B
m�B
n}B
n/B
ncB
n}B
nIB
ncB
nB
ncB
ncB
nB
m�B
m�B
n}B
pB
poB
p;B
p;B
p�B
p�B
qB
qB
q'B
p�B
qB
p�B
p�B
q[B
q�B
r�B
r�B
r�B
s�B
tB
t9B
tB
s�B
tTB
tB
tB
tB
tnB
t�B
t�B
t�B
uB
t�B
t�B
t�B
t�B
u%B
u�B
v�B
v�B
vFB
vzB
v�B
v�B
w2B
wLB
w�B
w�B
xB
xB
xRB
xlB
x�B
x�B
x�B
yrB
yrB
yrB
y�B
y�B
y�B
y�B
y�B
z^B
z�B
z�B
z�B
z�B
{dB
{�B
{�B
{B
{�B
{B
{�B
{�B
{�B
{�B
{�B
{�B
|B
{�B
|B
|B
|jB
|jB
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
}"B
}"B
|�B
}"B
}"B
}<B
}VB
}VB
}VB
}qB
}<B
}�B
}�B
~B
}�B
~B
~�B
~�B
~�B
.B
}B
�B
�B
�B
�B
��B
��B
��B
� B
� B
�UB
�;B
�UB
�oB
�oB
�oB
��B
��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104918  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604173749  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604173749  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604173749                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605023757  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605023757  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610131507                      G�O�G�O�G�O�                