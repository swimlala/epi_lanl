CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-06-19T17:09:03Z creation      
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
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �T   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �d   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �h   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �x   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �|   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200619170903  20220204114414  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               +A   AO  7662                            2C  D   APEX                            8312                            080318                          846 @ؔ��R��1   @ؔ�/hZ2@7z��vȴ�c�Z�11   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    +A   B   B   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B ffB  B  B  B   B(  B0  B8  B@ffBHffBP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0�fD1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:y�D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Do��Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt��Dy��D�qD�Z=D���D��
D�( D�S�D���D��fD��D�\�D���D��D�{D�UDڦD���D� D�d�D� D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�Q�@��R@��RA\)A?\)A_\)A\)A��A��A��A��AϮA߮A�B =pB�
B�
B�
B�
B'�
B/�
B7�
B@=pBH=pBO�
BW�
B_�
Bg�
Bo�
Bw�
B�
B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D }qD �qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD	}qD	�qD
}qD
�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qDwD�qD}qD�qD}qD�qDwD�qD}qD�qD}qD�qD}qD�qD}qD�qD }qD �qD!}qD!�qD"}qD"�qD#}qD#�qD$}qD$�qD%}qD%�qD&}qD&�qD'}qD'�qD(}qD(�qD)}qD)�qD*}qD*�qD+}qD+�qD,}qD,�qD-}qD-�qD.}qD.�qD/}qD/�qD0��D0�qD1}qD1�qD2}qD2�qD3}qD3�qD4}qD4�qD5}qD5�qD6}qD6�qD7}qD7�qD8}qD8�qD9}qD9�qD:wD:�qD;}qD;�qD<}qD<�qD=}qD=�qD>}qD>�qD?}qD?�qD@}qD@�qDA}qDA�qDB}qDB�qDC}qDC�qDD}qDD�qDE}qDE�qDF}qDF�qDG}qDG�qDH}qDH�qDI}qDI�qDJ}qDJ�qDK}qDK�qDL}qDL�qDM}qDM�qDN}qDN�qDO}qDO�qDP}qDP�qDQ}qDQ�qDR}qDR�qDS}qDS�qDT}qDT�qDU}qDU�qDV}qDV�qDW}qDW�qDX}qDX�qDY}qDY�qDZ}qDZ�qD[}qD[�qD\}qD\�qD]}qD]�qD^}qD^�qD_}qD_�qD`}qD`�qDa}qDa�qDb}qDb�qDc}qDc�qDd}qDd�qDe}qDe�qDf}qDf�qDg}qDg�qDh}qDh�qDi}qDi�qDj}qDj�qDk}qDk�qDl}qDl�qDm}qDm�qDn}qDn�qDo}qDo�Dp}qDp�qDq}qDq�qDr}qDr�qDs}qDs�qDt}qDt�>Dy� D�)D�X�D���D���D�&�D�R�D��fD��D�pD�[�D���D��fD�3D�S�Dڤ�D��{D��D�c�D�D�Ӆ111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AɁAɃAɃAɇ+AɓuAɑhAɏ\AɋDA�|�A�r�A�p�A�n�A�n�A�hsA�dZA�`BA�\)A�ZA�ZA�ZA�XA�VA�XA�XA�VA�S�A�Q�A�M�A�M�A�M�A�O�A�O�A�O�A�K�A�I�A�C�A�;dA��Aǝ�A��A���A��A�~�A��A�VA��A�ffA��A���A��^A���A��HA�C�A��`A�ffA��
A�oA�ƨA�?}A��PA�&�A��A�x�A��^A��A�bA��A�;dA��
A��A��7A���A���A�ffA�=qA��A�7LA�O�A�A�E�A���A�33A�z�A�
=A��DA�&�A�VA�v�A���A�dZA��#A�ZA�l�A�{A��A��A�?}A�dZA��A�O�A��;A�&�A���A�7LA��^A�VA���A���A�E�A�O�A���A��+A���A�Q�A�A��#A��DA���A}hsA{;dAz��Ay�Ax{Av�`AvM�Au/Ar��ApA�AoVAl�yAl��AlZAk�PAiAg�wAd�Ab�Aa;dA_�mA\�AZ�`AX��AW�#AV�9AT�AS�AP�HAO�7ALE�AHjAGXAG"�AG%AFZADZAC�PAB�RAA��AA%A?`BA<��A:M�A97LA8r�A7�FA5�FA3�A2^5A2JA1�PA1+A0��A/�#A/%A.5?A-��A-VA,��A+�A+�A*�A*M�A)��A(��A'�A%�A$��A$5?A#�-A!�A ��A�AA��A��A�`A9XA\)A9XA"�A�9AhsA�9A �A�yAE�A�A�!A�AbNAE�A��A+A��A^5A/A��A{A/A~�AA�A
r�A	��A	XAȴA=qA��AXA/A��AA�A�A��A��AK�@���@��!@�j@��@�J@���@�@��@�Ĝ@�l�@�@�5?@�Ĝ@�@���@�K�@���@�O�@ߝ�@ޗ�@��T@ݺ^@�X@�;d@��@���@�9X@�t�@�33@�ȴ@���@���@�
=@��#@�/@���@�j@�ƨ@�{@�`B@��m@�@�V@�Q�@�ƨ@��y@�O�@�  @���@î@Õ�@Å@�l�@�C�@�"�@��y@�`B@��9@�z�@�ƨ@�33@���@�J@�/@���@�r�@���@��y@�ff@��@�/@��@�Ĝ@�;d@�$�@��^@��/@���@���@���@�5?@��T@�p�@���@��@��
@�|�@�
=@�E�@��h@��D@�ƨ@�@�v�@��T@��@���@�Q�@�  @��F@�C�@���@�ȴ@��H@�ȴ@�M�@�5?@��^@���@�p�@�p�@�?}@��@�bN@�9X@��
@��H@�J@�/@�I�@� �@��@�t�@�t�@���@���@�ȴ@��@��H@��H@�v�@��@��#@��+@���@��@�ff@���@��-@�-@�$�@�G�@���@���@�Ĝ@�V@���@��@�r�@�r�@�bN@�Q�@�9X@�@���@�
=@�M�@�hs@��F@�ff@�?}@���@��9@���@�Q�@�A�@� �@��@�|�@�dZ@�33@�~�@��T@�X@�/@��j@���@��@���@�z�@�j@�bN@�  @���@���@���@�"�@�;d@�;d@���@�@�;d@��w@�(�@�l�@�ƨ@�b@� �@���@�t�@��w@�K�@�ȴ@�{@��@��#@��^@���@�O�@�O�@�hs@�`B@�X@�/@�7L@�?}@�?}@�V@��@���@���@���@��D@��@��u@��D@�z�@�j@�A�@��@��
@���@��P@�S�@��@��@�X@���@�{@��@��7@��@��9@��D@�r�@�j@�bN@�I�@� �@�(�@���@��m@���@��@~�@u��@i��@a�'@Z�@Q5�@Lh�@Fe@A�M@<�z@5L�@/��@)�T@$l"@��@�_@�P@.I@�@	�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  AɁAɃAɃAɇ+AɓuAɑhAɏ\AɋDA�|�A�r�A�p�A�n�A�n�A�hsA�dZA�`BA�\)A�ZA�ZA�ZA�XA�VA�XA�XA�VA�S�A�Q�A�M�A�M�A�M�A�O�A�O�A�O�A�K�A�I�A�C�A�;dA��Aǝ�A��A���A��A�~�A��A�VA��A�ffA��A���A��^A���A��HA�C�A��`A�ffA��
A�oA�ƨA�?}A��PA�&�A��A�x�A��^A��A�bA��A�;dA��
A��A��7A���A���A�ffA�=qA��A�7LA�O�A�A�E�A���A�33A�z�A�
=A��DA�&�A�VA�v�A���A�dZA��#A�ZA�l�A�{A��A��A�?}A�dZA��A�O�A��;A�&�A���A�7LA��^A�VA���A���A�E�A�O�A���A��+A���A�Q�A�A��#A��DA���A}hsA{;dAz��Ay�Ax{Av�`AvM�Au/Ar��ApA�AoVAl�yAl��AlZAk�PAiAg�wAd�Ab�Aa;dA_�mA\�AZ�`AX��AW�#AV�9AT�AS�AP�HAO�7ALE�AHjAGXAG"�AG%AFZADZAC�PAB�RAA��AA%A?`BA<��A:M�A97LA8r�A7�FA5�FA3�A2^5A2JA1�PA1+A0��A/�#A/%A.5?A-��A-VA,��A+�A+�A*�A*M�A)��A(��A'�A%�A$��A$5?A#�-A!�A ��A�AA��A��A�`A9XA\)A9XA"�A�9AhsA�9A �A�yAE�A�A�!A�AbNAE�A��A+A��A^5A/A��A{A/A~�AA�A
r�A	��A	XAȴA=qA��AXA/A��AA�A�A��A��AK�@���@��!@�j@��@�J@���@�@��@�Ĝ@�l�@�@�5?@�Ĝ@�@���@�K�@���@�O�@ߝ�@ޗ�@��T@ݺ^@�X@�;d@��@���@�9X@�t�@�33@�ȴ@���@���@�
=@��#@�/@���@�j@�ƨ@�{@�`B@��m@�@�V@�Q�@�ƨ@��y@�O�@�  @���@î@Õ�@Å@�l�@�C�@�"�@��y@�`B@��9@�z�@�ƨ@�33@���@�J@�/@���@�r�@���@��y@�ff@��@�/@��@�Ĝ@�;d@�$�@��^@��/@���@���@���@�5?@��T@�p�@���@��@��
@�|�@�
=@�E�@��h@��D@�ƨ@�@�v�@��T@��@���@�Q�@�  @��F@�C�@���@�ȴ@��H@�ȴ@�M�@�5?@��^@���@�p�@�p�@�?}@��@�bN@�9X@��
@��H@�J@�/@�I�@� �@��@�t�@�t�@���@���@�ȴ@��@��H@��H@�v�@��@��#@��+@���@��@�ff@���@��-@�-@�$�@�G�@���@���@�Ĝ@�V@���@��@�r�@�r�@�bN@�Q�@�9X@�@���@�
=@�M�@�hs@��F@�ff@�?}@���@��9@���@�Q�@�A�@� �@��@�|�@�dZ@�33@�~�@��T@�X@�/@��j@���@��@���@�z�@�j@�bN@�  @���@���@���@�"�@�;d@�;d@���@�@�;d@��w@�(�@�l�@�ƨ@�b@� �@���@�t�@��w@�K�@�ȴ@�{@��@��#@��^@���@�O�@�O�@�hs@�`B@�X@�/@�7L@�?}@�?}@�V@��@���@���@���@��D@��@��u@��D@�z�@�j@�A�@��@��
@���@��P@�S�@��@��@�X@���@�{@��@��7@��@��9@��D@�r�@�j@�bN@�I�@� �@�(�@���@��mG�O�@��@~�@u��@i��@a�'@Z�@Q5�@Lh�@Fe@A�M@<�z@5L�@/��@)�T@$l"@��@�_@�P@.I@�@	�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBu�Bv�Bv�Bw�B~�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�%B�%B�%B�+B�+B�+B�1B�1B�1B�1B�1B�1B�1B�1B�B~�Bn�Bt�Bw�Bt�Bu�Bu�By�Bz�Bx�By�B�B�7B�7B�7B�DB�JB�VB�VB�DB�=B�1B�B�B�B�B�B�B�B� B}�By�Bs�BjBZBXBXBVBC�B8RB0!B+B&�B �B{BPB%B  B��B�`B�#B��B��BǮB��B�RB��B��B�=Bm�BJ�B=qB6FB.B,B �B�B\B
��B
��B
�B
�B
��B
�}B
�FB
�B
��B
�PB
�B
{�B
_;B
D�B
>wB
8RB
.B
%�B
�B
�B
VB	��B	��B	�yB	�`B	�NB	�5B	��B	B	�B	��B	�DB	�B	m�B	^5B	N�B	B�B	<jB	(�B	�B	�B	�B��B�BB��B��B��BɺB�qB�jB�dB�'B�B��B�oB�+B�%B�B�B}�By�Bs�Bo�Bo�Bl�Bl�Bk�BjBk�Bk�Bk�Bk�Bl�BjBjBhsBhsBhsBffBhsBe`BdZBcTBcTB`BBbNBaHB_;B]/B\)B[#BYBXBW
BT�BT�BQ�BR�BP�BP�BN�BM�BM�BM�BL�BL�BK�BJ�BH�BI�BF�BF�BE�BE�BD�BC�BB�BB�BA�BA�B@�B@�B?}B?}B>wB>wB>wB;dB<jB:^B;dB9XB7LB7LB7LB5?B9XB8RB8RB8RB;dB;dB;dB<jB>wB>wB?}B@�BA�BA�BA�B@�B@�BB�BB�BB�BC�BB�BC�BC�BD�BD�BG�BH�BH�BH�BH�BJ�BN�BM�BQ�BS�BS�BVBW
BYB^5BaHBaHBaHBbNBbNBbNBcTBcTBbNBiyBiyBiyBl�Bm�Bo�Bq�Bt�Bt�Bt�Bw�By�Bz�B{�B{�B|�B~�B�B�B�B�+B�7B�DB�DB�VB�hB�uB��B��B��B��B��B��B��B�B�-B�?B�RB�^B�qB�}BBŢBȴB��B��B�/B�TB�ZB�B�B��B��B	B	VB	{B	{B	{B	�B	�B	�B	{B	�B	�B	#�B	#�B	"�B	&�B	'�B	+B	.B	33B	8RB	9XB	:^B	:^B	>wB	F�B	I�B	L�B	O�B	P�B	Q�B	S�B	YB	VB	R�B	S�B	\)B	aHB	e`B	hsB	k�B	o�B	o�B	o�B	o�B	q�B	s�B	w�B	z�B	z�B	x�B	t�B	r�B	r�B	r�B	s�B	w�B	z�B	{�B	~�B	� B	�B	�B	�%B	�+B	�+B	�1B	�+B	�B	}�B	{�B	|�B	� B	� B	�B	�B	�B	�%B	�%B	�1B	�DB	�oB	�{B	��B	��B	��B	��B	��B	�B	�!B	�-B	�3B	�LB	�LB	�FB	�9B	�FB	�XB	�dB	�jB	�qB	��B	ÖB	ĜB	ƨB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�#B	�B	�#B	�)B	�/B	�/B	�5B	�HB	�ZB	�`B	�fB	�mB	�sB	�B	�XB
B
	�B
�B
 �B
(�B
/iB
8RB
<PB
@�B
H�B
O�B
T�B
ZB
]dB
a�B
eFB
k�B
p�B
t�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  BmMBnRBnRBoXBv�Bx�Bx�By�B{�B|�B|�B|�B|�B|�B{�B{�B{�B|�B|�B|�B|�B|�B|�B}�B}�B}�B~�B~�B~�B�B�B�B�B�B�B�B�B|�Bv�Bf#BlGBo[BlHBmOBmOBqgBrmBpaBqgBz�B��B��B��B��B��B��B��B��B��B�B|�B|�Bx�By�Bx�Bz�B{�Bw�Bu�BqjBkEBbBQ�BO�BO�BM�B;)B/�B'�B"�B~BZBB�B��B��B�UB��BҾB˓B�iB�JB� B��B�|B�]B��Be4BBfB5B-�B%�B#�BnB0BB
�B
�{B
�DB
��B
ɚB
�-B
��B
��B
�dB
�B
{�B
s�B
V�B
<UB
60B
0B
%�B
�B
zB
CB
B	��B	�B	�9B	� B	�B	��B	ƛB	�RB	��B	�YB	�B	x�B	eZB	U�B	F�B	:[B	47B	 �B	�B	jB	QB�B�B��BǴBŨB��B�HB�AB�;B��B��B��B�IBB~ B{�By�Bu�Bq�Bk�Bg{Bg{BdiBdiBccBb]BccBccBccBccBdiBb]Bb]B`RB`RB`RB^EB`RB]?B\9B[4B[4BX"BZ.BY(BWBUBT
BSBP�BO�BN�BL�BL�BI�BJ�BH�BH�BF�BE�BE�BE�BD�BD�BC�BB�B@�BA�B>�B>�B=�B=�B<�B;zB:sB:sB9nB9nB8hB8hB7bB7bB6\B6\B6\B3JB4PB2DB3JB1>B/3B/3B/3B-&B1?B09B09B09B3KB3KB3KB4QB6^B6^B7dB8jB9qB9qB9qB8kB8kB:wB:wB:wB;~B:wB;~B;~B<�B<�B?�B@�B@�B@�B@�BB�BF�BE�BI�BK�BK�BM�BN�BP�BVBY/BY/BY/BZ5BZ5BZ5B[;B[;BZ5Ba`Ba`Ba`BdrBexBg�Bi�Bl�Bl�Bl�Bo�Bq�Br�Bs�Bs�Bt�Bv�Bz�Bz�B{�BB�B�*B�*B�<B�MB�ZB�lB��B��B��B��B��B��B��B�B�#B�5B�AB�TB�`B�rB��B��BéBƻB�B�5B�;B�fB�B�B��B��B	4B	YB	YB	YB	_B	_B	_B	YB	qB	�B	�B	�B	�B	�B	�B	"�B	%�B	+B	0.B	14B	2:B	2:B	6SB	>�B	A�B	D�B	G�B	H�B	I�B	K�B	P�B	M�B	J�B	K�B	TB	Y"B	]:B	`LB	c^B	gwB	gwB	gwB	gwB	i�B	k�B	o�B	r�B	r�B	p�B	l�B	j�B	j�B	j�B	k�B	o�B	r�B	s�B	v�B	w�B	x�B	z�B	}�B	B	B	�	B	B	x�B	u�B	s�B	t�B	w�B	w�B	x�B	x�B	y�B	}�B	}�B	�
B	�B	�GB	�SB	�eB	��B	��B	��B	��B	��B	��B	�B	�	B	�"B	�"B	�B	�B	�B	�.B	�9B	�?B	�FB	�XB	�kB	�qB	�}B	��B	��B	B	B	ġB	ŧB	ŧB	ƭB	ŧB	ƭB	ǳB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ƮB	ȹB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�	B	�B	�-B	�3B	�9B	�@G�O�B	��B	�*B	��B
�B
�B
�B
 xB
':B
0"B
4 B
8�B
@�B
G{B
L�B
Q�B
U3B
YeB
]B
cnB
h�B
l�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.04 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9998(+/-0.0001), vertically averaged dS =-0.008(+/-0.002) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144152022020411441520220204114415  AO  ARCAADJP                                                                    20200619170903    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200619170903  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200619170903  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114415  IP                  G�O�G�O�G�O�                