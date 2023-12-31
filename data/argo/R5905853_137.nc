CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-11-09T12:41:01Z creation;2022-11-09T12:41:02Z conversion to V3.1      
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
_FillValue                 �  I<   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M,   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �@   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �0   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �D   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ޼   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �    HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �$   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �(   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �h   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �x   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �|   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20221109124101  20221109125744  5905853                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA                                  2B  A   APEX                            8421                            2.11.2                          846 @���F)��1   @����<M^@/�t��cK-1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@y��@���A   A!��A@  A`  A�  A�  A�  A�  A�  A�  A�33A�  B   B  B  B  B   B(  B0  B:  B?33BH��BO��BW33B_��Bh  Bp��Bz  B}��B�  B�  B�  B�  B�  B�  B�ffB�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�ffB�33B���B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$�C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ�C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn�Cp�Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|fD|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @
>@w
>@��@��RA ��A?\)A_\)A\)A��A��A��A��AϮA��GA�A��B�
B�
B�
B�
B'�
B/�
B9�
B?
=BH��BOp�BW
=B_p�Bg�
Bp��By�
B}p�B��B��B��B��B��B��B�Q�B��B��RB��B��B��B��B��B��B��B��B��B��B��B��B��B��B�Q�B��B�RB��B��B��B��B��B��C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C$]C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CZ]C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cn]Cp]Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D }qD �qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD	}qD	�qD
}qD
�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD��D�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD }qD �qD!}qD!�qD"}qD"�qD#}qD#�qD$}qD$�qD%}qD%�qD&}qD&�qD'}qD'�qD(}qD(�qD)}qD)�qD*}qD*�qD+}qD+�qD,}qD,�qD-}qD-�qD.}qD.�qD/}qD/�qD0}qD0�qD1}qD1�qD2}qD2�qD3}qD3�qD4}qD4�qD5}qD5�qD6}qD6�qD7}qD7�qD8}qD8�qD9}qD9�qD:}qD:�qD;}qD;�qD<}qD<�qD=}qD=�qD>}qD>�qD?}qD?�qD@}qD@�qDA}qDA�qDB}qDB�qDC}qDC�qDD}qDD�qDE}qDE�qDF}qDF�qDG}qDG�qDH}qDH�qDI}qDI�qDJ}qDJ�qDK}qDK�qDL}qDL�qDM}qDM�qDN}qDN�qDO}qDO�qDP}qDP�qDQ}qDQ�qDR}qDR�qDS}qDS�qDT}qDT�qDU}qDU�qDV}qDV�qDW}qDW�qDX}qDX�qDY}qDY�qDZ}qDZ�qD[}qD[�qD\}qD\�qD]}qD]�qD^}qD^�qD_}qD_�qD`}qD`�qDa}qDa�qDb}qDb�qDc}qDc�qDd}qDd�qDe}qDe�qDf}qDf�qDg}qDg�qDh}qDh�qDi}qDi�qDj}qDj�qDk}qDk�qDl}qDl�qDm}qDm�qDn}qDn�qDo}qDo�qDp}qDp�qDq}qDq�qDr}qDr�qDs}qDs�qDt}qDt�qDu}qDu�qDv}qDv�qDw}qDw�qDx}qDx�qDy}qDy�qDz}qDz�qD{}qD|�D|}qD|�qD}}qD}�qD~}qD~�qD}qD�qD�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�;�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D¾�D���D�>�D�~�Dþ�D���D�>�D�~�Dľ�D���D�>�D�~�Dž�D���D�>�D�~�Dƾ�D���D�>�D�~�DǾ�D���D�>�D�~�DȾ�D���D�>�D�~�Dɾ�D���D�>�D�~�Dʾ�D���D�>�D�~�D˾�D���D�>�D�~�D̾�D���D�>�D�~�D;�D���D�>�D�~�Dξ�D���D�>�D�~�DϾ�D���D�>�D�~�Dо�D���D�>�D�~�DѾ�D���D�>�D�~�DҾ�D���D�>�D�~�DӾ�D���D�>�D�~�DԾ�D���D�>�D�~�Dվ�D���D�>�D�~�D־�D���D�>�D�~�D׾�D���D�>�D�~�Dؾ�D���D�>�D�~�Dپ�D���D�>�D�~�Dھ�D���D�>�D�~�D۾�D���D�>�D�~�Dܾ�D���D�>�D�~�Dݾ�D���D�>�D�~�D޾�D���D�>�D�~�D߾�D���D�>�D�~�DྸD���D�>�D�~�DᾸD���D�>�D�~�D⾸D���D�>�D�~�D㾸D���D�>�D�~�D侸D���D�>�D�~�D徸D���D�>�D�~�D澸D���D�>�D�~�D羸D���D�>�D�~�D辸D���D�>�D�~�D龸D���D�>�D�~�D꾸D���D�>�D�~�D뾸D���D�>�D�~�D쾸D���D�>�D�~�D���D���D�>�D�~�DD���D�>�D�~�DﾸD���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A���A���A���A���A���A���A��JA��A��A���A��VA���A��A��A��A��A��A��A��A��A��A��A���A�T�A��A�E9A�B�A�T�A���A��|A��A�33A���A��'A�ݘA��A���A���A��'A��~A�A�R�A�VA���A��A���A��?A�ȴA���A��A�DA���A��A� �A�l�A�iA�"4A�A���A��A�)_A�jA�z�A���A�v�A�]�A��*A���A��A��A��7A��zA���A��A��zA�˒A��A��0Ax�AAtn/ApE9AkiDAiJAd�Aa�nA]��A[T�AX:�AV(�ATW�AR��AO�AMm�AJ��AI��AF�~AD4nAA��A@�XA>^5A<FtA;�A9m�A7�A5��A4��A3m�A2k�A2e,A2�A1�?A1L0A0�A.Z�A,�A+�FA)��A(��A(	A(��A*�MA*� A)��A'��A&�!A%��A%�A$�rA% iA%1A%&A$e,A!�7A u�A �AO�A��A,�Ap;A8�A�A�=An�A�`A�A��Ay�A��A�A��Aj�A�sA��A&�A�AA�ATaAxA��A�A�hA�-A��A�/A��A9XA�'A	lA�?A��A��AVmA.IA
�cA	��A��At�A[WA	 iA	+Aq�A�A�~A�A�NA��ArGAxAB[A�AAϫA�A��Am]A+AԕA��A�A��An/A6�A_A��A ��A m�@�k�@��c@���@�/@��.@���@���@��@�l�@�e�@��@�[W@�u�@��#@���@�*@��@��z@�H�@���@�V�@��@��[@�9�@��j@��@��@��@��@��@��5@��]@���@�_@㰊@�RT@�Ĝ@�6@�k@�A�@�!@��@��@ޤ�@�a|@��@���@݀4@�4�@��@��M@���@��`@�	l@�"�@��@܅�@��@�a�@��@ڿ�@ڃ@�v�@�/�@ٟV@�Mj@���@��@��m@�H@���@նF@�4�@�ں@�;�@�RT@�l�@��M@Ҍ�@�/�@у{@�T�@��@�h
@�خ@ϐ�@��@Ζ�@�v�@�Ov@��@ͼ�@�+@̕@�~@��@��@�y�@�l�@�C�@�o@�y>@��g@�Dg@��H@ƍ�@�c�@Ĥ�@�!@×$@�O@�@���@���@��@��P@�33@��@��5@��@��@���@�F�@��p@���@�C�@�.�@��@���@�Dg@���@��.@�_@� �@���@��d@���@�IR@�.I@���@���@��K@�{J@�N<@��@��]@��}@��@�X@�)_@�ߤ@�y>@��.@���@�Y�@��@��@���@���@���@��D@���@�s�@�hs@�&�@�S@��x@�+k@��@���@�7L@�!�@��@��Y@��@��R@��z@�}V@� �@�� @��@��^@��@�S�@��[@��`@���@��F@��Z@��@���@��D@��v@�c�@�H@�0U@���@��@��@��v@��B@�o@�rG@���@�7L@���@��b@��#@��:@�A @���@���@�_�@�1@��@��)@�c�@��@���@�v`@�5�@���@��@���@�oi@�$@��)@���@���@�[W@�9�@��@��@���@�x@�خ@��k@�/�@���@���@�l�@�#:@���@�Z�@�%@��f@��@���@���@�D�@�6�@��@��@��7@�:�@�@���@���@�?@�8�@�6�@��@��T@���@�Z�@��@���@��9@�J�@�4n@�u@��a@�F�@���@��1@�n�@�d�@�Ov@�;�@�,=@���@�J#@�:�@�*0@��@��R@�I�@�!@��@��$@�o�@�8@��@��!@��Y@�bN@�@���@��=@�T�@��@��j@���@�W�@�8�@�@��+@��g@��7@�K�@�*0@���@��@�e�@�H�@�b@���@�F@�+@��@���@�Ĝ@�q@��@��N@���@��V@���@�_p@��@���@�j@� �@��>@�x�@�>�@��@��c@���@��@�l"@�D�@�1'@�!�@��@�� @��V@��:@�j@�33@���@�Ɇ@��@��Y@�z@�a|@�3�@��.@��&@��P@�L�@�(�@��@���@��!@���@�oi@�K^@�-@��@�@�@~�@~��@~;�@}�N@}8�@|��@|r�@| �@{�@z�"@z��@z&�@y�@y��@yY�@x�v@xy>@x1@w�0@v��@v#:@u�H@u�@t�O@tS�@s�$@r�@r{�@r�@q��@qu�@q+@pU2@o��@n�h@n�@m��@lbN@k�@kX�@k+@j�@jkQ@j�@i��@if�@hѷ@g�r@gl�@f��@fTa@e��@e��@e�@d��@d�@d2�@c��@b�"@b��@bL0@a��@a�@aVm@`��@`�@_�:@^�b@^0U@]c@]�@\�@[�r@[iD@[�@Z�}@Z:*@Z	@Y��@Y��@Y=�@X��@X7�@W��@W�{@W"�@V��@V�}@Vp;@U�@Us�@U4@Tz�@T%�@S��@S~�@S)_@R�B@RC�@Q�.@Q�z@Qs�@Q0�@P�P@P��@P�@P$@O�a@O�@O;d@O�@N��@M��@Mp�@L�U@LXy@K�Q@Kb�@K.I@J�y@J��@J� @Ji�@J&�@I�@I��@IrG@I#�@H�v@Hl"@H�@G��@GC�@F��@F��@F�+@Fs�@Fu@E�'@EA @D�@D��@D'R@C�[@CP�@C>�@C�@B�b@B:*@A�D@Aϫ@A�@AN<@A%@@�D@@_@@9X@@!@@x@?��@?P�@?�@>�2@>{�@>5?@=ϫ@=�C@=�'@=c@=k�@=`B@=�@<Ĝ@<j@<<�@<b@;��@;��@;.I@:�h@:	@9}�@9�@8��@86@7��@6�M@6�}@6V@6�@5�o@5��@5k�@5	l@4��@4�@3�
@3��@3��@3|�@3RT@2�R@2��@2�+@2V@2�@1�@1�@1�~@14@0�@0�$@0`�@/s@/$t@.�@.{�@.u@-�@-��@-m]@-x�@-?}@-�@,֡@,c�@,Ft@,4n@,%�@,�@,�@,	�@+��@+�0@+X�@+8@+Y@*�,@*��@*�b@*s�@* �@)�@)c�@)7L@)%F@(�@(Ɇ@(�?@(�?@(�j@(�I@(~(@(Xy@(	�@'�0@'U�@&��@&��@&u%@&5?@%�.@%��@%��@%|@%&�@$�)@$��@$V�@$V�@$H@$ �@#��@#�	@#dZ@#9�@#@"�2@"�'@"�F@"\�@"3�@!�.@!ԕ@!�'@!��@!A @!�@ �v@ ��@ $@�]@��@{J@j�@K�@&@��@n�@8�@��@�3@��@c�@S&@/@�E@h�@��@�a@��@�{@8@�,@��@��@kQ@�@�@��@��@:�@�v@w�@<�@�A@��@j�@1�@ i@�s@�@��@v�@i�@_�@@�@��@N<@-w@!�@ \@q@�P@��@�Y@r�@`�@�@�@�]@� @��@g�@33@�8@�}@a|@C�@:*@4@ԕ@��@u�@\�@S&@O�@@�@�?@��@�D@-�@��@�k@b�@
=@�L@YK@M�@J�@E�@:*@0U@0U@$�@�@��@�3@�X@rG@=�@�@�|@��@�@<�@%�@�@	�@  @�+@ݘ@ƨ@��@��@�f@a@F�@@O@!-@
�y@
�m@
��@
��@
��@
s�@
ff@
E�@
J@	�#@	��@	Q�@	/@�@�@�z@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A���A���A���A���A���A���A��JA��A��A���A��VA���A��A��A��A��A��A��A��A��A��A��A���A�T�A��A�E9A�B�A�T�A���A��|A��A�33A���A��'A�ݘA��A���A���A��'A��~A�A�R�A�VA���A��A���A��?A�ȴA���A��A�DA���A��A� �A�l�A�iA�"4A�A���A��A�)_A�jA�z�A���A�v�A�]�A��*A���A��A��A��7A��zA���A��A��zA�˒A��A��0Ax�AAtn/ApE9AkiDAiJAd�Aa�nA]��A[T�AX:�AV(�ATW�AR��AO�AMm�AJ��AI��AF�~AD4nAA��A@�XA>^5A<FtA;�A9m�A7�A5��A4��A3m�A2k�A2e,A2�A1�?A1L0A0�A.Z�A,�A+�FA)��A(��A(	A(��A*�MA*� A)��A'��A&�!A%��A%�A$�rA% iA%1A%&A$e,A!�7A u�A �AO�A��A,�Ap;A8�A�A�=An�A�`A�A��Ay�A��A�A��Aj�A�sA��A&�A�AA�ATaAxA��A�A�hA�-A��A�/A��A9XA�'A	lA�?A��A��AVmA.IA
�cA	��A��At�A[WA	 iA	+Aq�A�A�~A�A�NA��ArGAxAB[A�AAϫA�A��Am]A+AԕA��A�A��An/A6�A_A��A ��A m�@�k�@��c@���@�/@��.@���@���@��@�l�@�e�@��@�[W@�u�@��#@���@�*@��@��z@�H�@���@�V�@��@��[@�9�@��j@��@��@��@��@��@��5@��]@���@�_@㰊@�RT@�Ĝ@�6@�k@�A�@�!@��@��@ޤ�@�a|@��@���@݀4@�4�@��@��M@���@��`@�	l@�"�@��@܅�@��@�a�@��@ڿ�@ڃ@�v�@�/�@ٟV@�Mj@���@��@��m@�H@���@նF@�4�@�ں@�;�@�RT@�l�@��M@Ҍ�@�/�@у{@�T�@��@�h
@�خ@ϐ�@��@Ζ�@�v�@�Ov@��@ͼ�@�+@̕@�~@��@��@�y�@�l�@�C�@�o@�y>@��g@�Dg@��H@ƍ�@�c�@Ĥ�@�!@×$@�O@�@���@���@��@��P@�33@��@��5@��@��@���@�F�@��p@���@�C�@�.�@��@���@�Dg@���@��.@�_@� �@���@��d@���@�IR@�.I@���@���@��K@�{J@�N<@��@��]@��}@��@�X@�)_@�ߤ@�y>@��.@���@�Y�@��@��@���@���@���@��D@���@�s�@�hs@�&�@�S@��x@�+k@��@���@�7L@�!�@��@��Y@��@��R@��z@�}V@� �@�� @��@��^@��@�S�@��[@��`@���@��F@��Z@��@���@��D@��v@�c�@�H@�0U@���@��@��@��v@��B@�o@�rG@���@�7L@���@��b@��#@��:@�A @���@���@�_�@�1@��@��)@�c�@��@���@�v`@�5�@���@��@���@�oi@�$@��)@���@���@�[W@�9�@��@��@���@�x@�خ@��k@�/�@���@���@�l�@�#:@���@�Z�@�%@��f@��@���@���@�D�@�6�@��@��@��7@�:�@�@���@���@�?@�8�@�6�@��@��T@���@�Z�@��@���@��9@�J�@�4n@�u@��a@�F�@���@��1@�n�@�d�@�Ov@�;�@�,=@���@�J#@�:�@�*0@��@��R@�I�@�!@��@��$@�o�@�8@��@��!@��Y@�bN@�@���@��=@�T�@��@��j@���@�W�@�8�@�@��+@��g@��7@�K�@�*0@���@��@�e�@�H�@�b@���@�F@�+@��@���@�Ĝ@�q@��@��N@���@��V@���@�_p@��@���@�j@� �@��>@�x�@�>�@��@��c@���@��@�l"@�D�@�1'@�!�@��@�� @��V@��:@�j@�33@���@�Ɇ@��@��Y@�z@�a|@�3�@��.@��&@��P@�L�@�(�@��@���@��!@���@�oi@�K^@�-@��@�@�@~�@~��@~;�@}�N@}8�@|��@|r�@| �@{�@z�"@z��@z&�@y�@y��@yY�@x�v@xy>@x1@w�0@v��@v#:@u�H@u�@t�O@tS�@s�$@r�@r{�@r�@q��@qu�@q+@pU2@o��@n�h@n�@m��@lbN@k�@kX�@k+@j�@jkQ@j�@i��@if�@hѷ@g�r@gl�@f��@fTa@e��@e��@e�@d��@d�@d2�@c��@b�"@b��@bL0@a��@a�@aVm@`��@`�@_�:@^�b@^0U@]c@]�@\�@[�r@[iD@[�@Z�}@Z:*@Z	@Y��@Y��@Y=�@X��@X7�@W��@W�{@W"�@V��@V�}@Vp;@U�@Us�@U4@Tz�@T%�@S��@S~�@S)_@R�B@RC�@Q�.@Q�z@Qs�@Q0�@P�P@P��@P�@P$@O�a@O�@O;d@O�@N��@M��@Mp�@L�U@LXy@K�Q@Kb�@K.I@J�y@J��@J� @Ji�@J&�@I�@I��@IrG@I#�@H�v@Hl"@H�@G��@GC�@F��@F��@F�+@Fs�@Fu@E�'@EA @D�@D��@D'R@C�[@CP�@C>�@C�@B�b@B:*@A�D@Aϫ@A�@AN<@A%@@�D@@_@@9X@@!@@x@?��@?P�@?�@>�2@>{�@>5?@=ϫ@=�C@=�'@=c@=k�@=`B@=�@<Ĝ@<j@<<�@<b@;��@;��@;.I@:�h@:	@9}�@9�@8��@86@7��@6�M@6�}@6V@6�@5�o@5��@5k�@5	l@4��@4�@3�
@3��@3��@3|�@3RT@2�R@2��@2�+@2V@2�@1�@1�@1�~@14@0�@0�$@0`�@/s@/$t@.�@.{�@.u@-�@-��@-m]@-x�@-?}@-�@,֡@,c�@,Ft@,4n@,%�@,�@,�@,	�@+��@+�0@+X�@+8@+Y@*�,@*��@*�b@*s�@* �@)�@)c�@)7L@)%F@(�@(Ɇ@(�?@(�?@(�j@(�I@(~(@(Xy@(	�@'�0@'U�@&��@&��@&u%@&5?@%�.@%��@%��@%|@%&�@$�)@$��@$V�@$V�@$H@$ �@#��@#�	@#dZ@#9�@#@"�2@"�'@"�F@"\�@"3�@!�.@!ԕ@!�'@!��@!A @!�@ �v@ ��@ $@�]@��@{J@j�@K�@&@��@n�@8�@��@�3@��@c�@S&@/@�E@h�@��@�a@��@�{@8@�,@��@��@kQ@�@�@��@��@:�@�v@w�@<�@�A@��@j�@1�@ i@�s@�@��@v�@i�@_�@@�@��@N<@-w@!�@ \@q@�P@��@�Y@r�@`�@�@�@�]@� @��@g�@33@�8@�}@a|@C�@:*@4@ԕ@��@u�@\�@S&@O�@@�@�?@��@�D@-�@��@�k@b�@
=@�L@YK@M�@J�@E�@:*@0U@0U@$�@�@��@�3@�X@rG@=�@�@�|@��@�@<�@%�@�@	�@  @�+@ݘ@ƨ@��@��@�f@a@F�@@O@!-@
�y@
�m@
��@
��@
��@
s�@
ff@
E�@
J@	�#@	��@	Q�@	/@�@�@�z@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BZBZBY�BY�BY�BY�BY�BYeBYeBYBY�BYBYBY�BY�BY�BZBY�BYBY�BaHBa�B��B��B
9>B
B
<�B
�OB
��B
��B
��B
�MB
�/BZ7B��Bc�BVB|�B��B�YB�oB��BwBd�B?B_BYBD�B�fB�XB�[B��B�TB�oB�AB�B�B��B�YB�BB�AB�%B��B�B��Br�Be�BT�B<jB"BUB
��B
�QB
qAB
P�B
>]B
+kB

�B	�B	�B	��B	x�B	k�B	Z7B	IB	3�B	($B	�B	NB	~B	zB��B�XB֡B��B�4B��B�AB�$B��B�B�B�B�WB��B�:B��B��B�)B�zB�<B�B�B޸BּB�VB�B�	BөB	'B	@�B	t�B	�NB	�EB	��B	��B	��B	� B	��B	�_B	��B	�B	�NB	�mB	�#B	��B	��B	��B	�EB	̈́B	�jB	��B	żB	�uB	ňB	��B	�B	��B	�\B	�"B	ϫB	��B	�B	�B	��B	��B	ƨB	�YB	��B	��B	�cB	�B	��B	�B	��B	�0B	�'B	�BB	׍B	רB	�?B	�gB	�:B	�uB	��B	�ZB	��B	��B	ɺB	�B	ںB	�B	��B	ҽB	�B	�YB	��B	�/B	�!B	��B	�B	��B	��B	��B	��B	��B	�B	�`B	��B	�B	�B	�B	�B	�B	�B	�yB	��B	�>B	�)B	�"B	��B	�B	��B	��B	�_B	�tB	�B	�fB	�ZB	�B	�,B	��B	�B	�tB	�:B	� B	�B	��B	��B	�B	��B	�]B	�dB	ݲB	��B	�B	�vB	�-B	�B	��B	�|B	�bB	��B	�'B	��B	��B	�B	�:B	�@B	�B	�B	��B	�8B	�B	�2B	�B	��B	�6B	�qB	�B	�vB	�|B	��B	�B	�vB	��B	�vB	��B	�B	��B	��B	�B	�B	�B	�B	�GB	��B	�B	�[B	�;B	�oB	�'B	�ZB	�`B	��B	��B	�`B	��B	��B	�LB	�`B	�?B	�?B	��B	�ZB	�`B	�LB	�zB	�tB	�TB	�MB	�B	�B	��B	�-B	�|B	�-B	��B	�'B	�oB	�;B	�B	� B	�cB	�/B	��B	��B	�wB	�B	�)B	�CB	��B	�B	�cB	�cB	��B	��B	�UB	�B	�'B	��B	�[B	�AB	�[B	�[B	��B	�B	�hB	��B	�B	�TB	�B	�B	�ZB	�ZB	��B	�FB	��B	�RB	�lB	�	B	�	B	�>B	�B	�B	�PB	��B	�B	��B	��B	�BB	�]B	�BB	�BB	�BB	�]B	��B	�VB	�VB	��B	�BB	�wB	�]B	�]B	�wB	��B	�wB	�]B	��B	�<B	�0B	�B	�jB	�"B	�B	�<B	�BB	��B	�}B
 �B
 iB
B
UB
UB
�B
 �B
 B	��B
 �B
�B
B
{B
B
YB
+B
�B
�B

rB
�B
bB
�B
�B
B
,B
�B
FB
{B
FB
�B
FB
&B
�B
[B
�B
NB
B
NB
�B
�B
aB
�B
uB
uB
B
�B
{B
�B
aB
�B
@B
�B
�B
�B
{B
gB
�B
mB
9B
�B
�B
?B
sB
YB
YB
�B
_B
yB
1B
eB
�B
B
B
7B
kB
�B
�B
�B
�B
	B
=B
�B
�B
]B
�B
/B
/B
dB
�B
5B
�B
!B
pB
;B
pB
VB
VB
 BB
 vB
 �B
 vB
 �B
!HB
!�B
!|B
!�B
"�B
"hB
"�B
"�B
#B
#TB
#�B
#�B
$&B
$tB
$�B
%`B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
&2B
&fB
&fB
&�B
&�B
'RB
'RB
'�B
(XB
(�B
(�B
(�B
(�B
(�B
)*B
)�B
)�B
*B
*�B
*eB
*�B
*�B
+B
+�B
,B
,WB
,�B
-CB
-)B
-�B
.}B
.�B
.�B
/5B
/iB
/5B
/OB
/�B
/�B
0!B
0oB
0�B
1B
1B
1[B
1�B
2B
2B
2GB
2B
1�B
2�B
2�B
2�B
3MB
3�B
3�B
3�B
4TB
4B
3�B
4�B
4�B
4�B
5�B
5�B
5�B
6+B
6B
6+B
6�B
6�B
7LB
7�B
7�B
8B
8lB
8B
88B
88B
8�B
9	B
9	B
9�B
9�B
9�B
:B
:�B
:xB
;B
;JB
;B
;�B
<B
<6B
<jB
<�B
=VB
>B
>(B
>wB
?cB
?�B
?�B
@B
A�B
BB
BAB
B�B
B�B
CB
C�B
C�B
C�B
DB
DMB
D�B
D3B
DB
DB
D3B
D�B
D�B
D�B
ESB
E�B
E�B
F�B
GB
G+B
G�B
HKB
H�B
H�B
IB
I�B
I�B
J�B
J�B
J�B
K�B
LdB
K�B
L0B
LJB
L�B
LdB
L�B
MB
MB
MB
M6B
MB
M�B
M�B
M�B
N�B
N�B
OBB
N�B
OBB
OvB
O�B
O�B
P.B
PbB
P�B
P�B
Q B
QNB
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
S@B
SuB
SuB
S�B
S�B
T{B
TaB
T�B
T�B
UMB
U2B
U�B
U�B
V9B
V9B
V�B
V�B
V�B
W$B
W?B
W?B
WsB
W?B
W�B
W�B
W�B
W�B
W�B
W�B
X+B
X�B
X�B
X�B
YKB
ZB
ZB
Z7B
Z�B
Z�B
Z�B
Z�B
[#B
[=B
[=B
[WB
[�B
[�B
[�B
\]B
\)B
\�B
]dB
]~B
]�B
^�B
^�B
^�B
_B
_VB
_�B
_�B
_�B
`B
`B
`BB
`�B
`�B
a-B
a�B
a�B
a|B
b�B
c�B
c:B
b�B
cnB
cnB
cTB
c�B
dtB
dZB
d�B
eB
e,B
eFB
e`B
eFB
e�B
e�B
e�B
f2B
f2B
ffB
ffB
f�B
g8B
g8B
g8B
gRB
g�B
h$B
hXB
h�B
h�B
i*B
iB
i�B
h�B
i�B
i�B
jeB
jKB
jB
j�B
j�B
j�B
jB
jB
j�B
kB
kB
kB
k6B
kkB
k�B
k�B
k�B
k�B
k�B
l=B
lqB
lB
l�B
l=B
l�B
l�B
l�B
l�B
l�B
l�B
mCB
m]B
m�B
nB
nIB
ncB
n�B
o B
oB
n�B
o5B
oiB
o�B
o�B
pUB
pB
pUB
p;B
poB
p�B
p�B
q'B
q'B
q[B
q[B
q�B
q�B
q�B
rB
rB
q�B
rB
rB
r�B
r|B
r�B
sB
s3B
tB
s�B
s�B
tB
s�B
tTB
t�B
t�B
uB
utB
uZB
u�B
u�B
u�B
v�B
vzB
v�B
w2B
v�B
w2B
w�B
w�B
w�B
xB
xB
xlB
x�B
x�B
x�B
y$B
yXB
y�B
y�B
zDB
z�B
z�B
z�B
{B
{B
{0B
{JB
{JB
{dB
{dB
{�B
{�B
{�B
|6B
|6B
|jB
|PB
|jB
|�B
|�B
}"B
}"B
}<B
}�B
}�B
}�B
}�B
}�B
~BB
~BB
~�B
~�B
.B
cB
HB
cB
�B
�B
�B
�OB
�4B
�B
��B
��B
��B
��B
��B
�;B
��B
��B
�B
�[B
��B
�B
�-B
�-B
�-B
�GB
�GB
�-B
�GB
�GB
�aB
��B
��B
��B
�3B
�gB
�gB
��B
��B
�9B
�SB
�9B
�SB
�SB
�mB
�mB
��B
��B
��B
��B
��B
�B
��B
�B
�YB
��B
��B
��B
��B
��B
��B
��B
�B
�+B
��B
��B
�B
�1B
��B
��B
��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BZBZBY�BY�BY�BY�BY�BYeBYeBYBY�BYBYBY�BY�BY�BZBY�BYBY�BaHBa�B��B��B
9>B
B
<�B
�OB
��B
��B
��B
�MB
�/BZ7B��Bc�BVB|�B��B�YB�oB��BwBd�B?B_BYBD�B�fB�XB�[B��B�TB�oB�AB�B�B��B�YB�BB�AB�%B��B�B��Br�Be�BT�B<jB"BUB
��B
�QB
qAB
P�B
>]B
+kB

�B	�B	�B	��B	x�B	k�B	Z7B	IB	3�B	($B	�B	NB	~B	zB��B�XB֡B��B�4B��B�AB�$B��B�B�B�B�WB��B�:B��B��B�)B�zB�<B�B�B޸BּB�VB�B�	BөB	'B	@�B	t�B	�NB	�EB	��B	��B	��B	� B	��B	�_B	��B	�B	�NB	�mB	�#B	��B	��B	��B	�EB	̈́B	�jB	��B	żB	�uB	ňB	��B	�B	��B	�\B	�"B	ϫB	��B	�B	�B	��B	��B	ƨB	�YB	��B	��B	�cB	�B	��B	�B	��B	�0B	�'B	�BB	׍B	רB	�?B	�gB	�:B	�uB	��B	�ZB	��B	��B	ɺB	�B	ںB	�B	��B	ҽB	�B	�YB	��B	�/B	�!B	��B	�B	��B	��B	��B	��B	��B	�B	�`B	��B	�B	�B	�B	�B	�B	�B	�yB	��B	�>B	�)B	�"B	��B	�B	��B	��B	�_B	�tB	�B	�fB	�ZB	�B	�,B	��B	�B	�tB	�:B	� B	�B	��B	��B	�B	��B	�]B	�dB	ݲB	��B	�B	�vB	�-B	�B	��B	�|B	�bB	��B	�'B	��B	��B	�B	�:B	�@B	�B	�B	��B	�8B	�B	�2B	�B	��B	�6B	�qB	�B	�vB	�|B	��B	�B	�vB	��B	�vB	��B	�B	��B	��B	�B	�B	�B	�B	�GB	��B	�B	�[B	�;B	�oB	�'B	�ZB	�`B	��B	��B	�`B	��B	��B	�LB	�`B	�?B	�?B	��B	�ZB	�`B	�LB	�zB	�tB	�TB	�MB	�B	�B	��B	�-B	�|B	�-B	��B	�'B	�oB	�;B	�B	� B	�cB	�/B	��B	��B	�wB	�B	�)B	�CB	��B	�B	�cB	�cB	��B	��B	�UB	�B	�'B	��B	�[B	�AB	�[B	�[B	��B	�B	�hB	��B	�B	�TB	�B	�B	�ZB	�ZB	��B	�FB	��B	�RB	�lB	�	B	�	B	�>B	�B	�B	�PB	��B	�B	��B	��B	�BB	�]B	�BB	�BB	�BB	�]B	��B	�VB	�VB	��B	�BB	�wB	�]B	�]B	�wB	��B	�wB	�]B	��B	�<B	�0B	�B	�jB	�"B	�B	�<B	�BB	��B	�}B
 �B
 iB
B
UB
UB
�B
 �B
 B	��B
 �B
�B
B
{B
B
YB
+B
�B
�B

rB
�B
bB
�B
�B
B
,B
�B
FB
{B
FB
�B
FB
&B
�B
[B
�B
NB
B
NB
�B
�B
aB
�B
uB
uB
B
�B
{B
�B
aB
�B
@B
�B
�B
�B
{B
gB
�B
mB
9B
�B
�B
?B
sB
YB
YB
�B
_B
yB
1B
eB
�B
B
B
7B
kB
�B
�B
�B
�B
	B
=B
�B
�B
]B
�B
/B
/B
dB
�B
5B
�B
!B
pB
;B
pB
VB
VB
 BB
 vB
 �B
 vB
 �B
!HB
!�B
!|B
!�B
"�B
"hB
"�B
"�B
#B
#TB
#�B
#�B
$&B
$tB
$�B
%`B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
&2B
&fB
&fB
&�B
&�B
'RB
'RB
'�B
(XB
(�B
(�B
(�B
(�B
(�B
)*B
)�B
)�B
*B
*�B
*eB
*�B
*�B
+B
+�B
,B
,WB
,�B
-CB
-)B
-�B
.}B
.�B
.�B
/5B
/iB
/5B
/OB
/�B
/�B
0!B
0oB
0�B
1B
1B
1[B
1�B
2B
2B
2GB
2B
1�B
2�B
2�B
2�B
3MB
3�B
3�B
3�B
4TB
4B
3�B
4�B
4�B
4�B
5�B
5�B
5�B
6+B
6B
6+B
6�B
6�B
7LB
7�B
7�B
8B
8lB
8B
88B
88B
8�B
9	B
9	B
9�B
9�B
9�B
:B
:�B
:xB
;B
;JB
;B
;�B
<B
<6B
<jB
<�B
=VB
>B
>(B
>wB
?cB
?�B
?�B
@B
A�B
BB
BAB
B�B
B�B
CB
C�B
C�B
C�B
DB
DMB
D�B
D3B
DB
DB
D3B
D�B
D�B
D�B
ESB
E�B
E�B
F�B
GB
G+B
G�B
HKB
H�B
H�B
IB
I�B
I�B
J�B
J�B
J�B
K�B
LdB
K�B
L0B
LJB
L�B
LdB
L�B
MB
MB
MB
M6B
MB
M�B
M�B
M�B
N�B
N�B
OBB
N�B
OBB
OvB
O�B
O�B
P.B
PbB
P�B
P�B
Q B
QNB
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
S@B
SuB
SuB
S�B
S�B
T{B
TaB
T�B
T�B
UMB
U2B
U�B
U�B
V9B
V9B
V�B
V�B
V�B
W$B
W?B
W?B
WsB
W?B
W�B
W�B
W�B
W�B
W�B
W�B
X+B
X�B
X�B
X�B
YKB
ZB
ZB
Z7B
Z�B
Z�B
Z�B
Z�B
[#B
[=B
[=B
[WB
[�B
[�B
[�B
\]B
\)B
\�B
]dB
]~B
]�B
^�B
^�B
^�B
_B
_VB
_�B
_�B
_�B
`B
`B
`BB
`�B
`�B
a-B
a�B
a�B
a|B
b�B
c�B
c:B
b�B
cnB
cnB
cTB
c�B
dtB
dZB
d�B
eB
e,B
eFB
e`B
eFB
e�B
e�B
e�B
f2B
f2B
ffB
ffB
f�B
g8B
g8B
g8B
gRB
g�B
h$B
hXB
h�B
h�B
i*B
iB
i�B
h�B
i�B
i�B
jeB
jKB
jB
j�B
j�B
j�B
jB
jB
j�B
kB
kB
kB
k6B
kkB
k�B
k�B
k�B
k�B
k�B
l=B
lqB
lB
l�B
l=B
l�B
l�B
l�B
l�B
l�B
l�B
mCB
m]B
m�B
nB
nIB
ncB
n�B
o B
oB
n�B
o5B
oiB
o�B
o�B
pUB
pB
pUB
p;B
poB
p�B
p�B
q'B
q'B
q[B
q[B
q�B
q�B
q�B
rB
rB
q�B
rB
rB
r�B
r|B
r�B
sB
s3B
tB
s�B
s�B
tB
s�B
tTB
t�B
t�B
uB
utB
uZB
u�B
u�B
u�B
v�B
vzB
v�B
w2B
v�B
w2B
w�B
w�B
w�B
xB
xB
xlB
x�B
x�B
x�B
y$B
yXB
y�B
y�B
zDB
z�B
z�B
z�B
{B
{B
{0B
{JB
{JB
{dB
{dB
{�B
{�B
{�B
|6B
|6B
|jB
|PB
|jB
|�B
|�B
}"B
}"B
}<B
}�B
}�B
}�B
}�B
}�B
~BB
~BB
~�B
~�B
.B
cB
HB
cB
�B
�B
�B
�OB
�4B
�B
��B
��B
��B
��B
��B
�;B
��B
��B
�B
�[B
��B
�B
�-B
�-B
�-B
�GB
�GB
�-B
�GB
�GB
�aB
��B
��B
��B
�3B
�gB
�gB
��B
��B
�9B
�SB
�9B
�SB
�SB
�mB
�mB
��B
��B
��B
��B
��B
�B
��B
�B
�YB
��B
��B
��B
��B
��B
��B
��B
�B
�+B
��B
��B
�B
�1B
��B
��B
��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20221109124042  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20221109124101  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20221109124102  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20221109124102                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20221109214106  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20221109214106  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20221109125744                      G�O�G�O�G�O�                