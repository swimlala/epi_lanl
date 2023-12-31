CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-06-19T17:08:57Z creation      
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
_FillValue                    �`Argo profile    3.1 1.2 19500101000000  20200619170857  20220204114412  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  7662                            2C  D   APEX                            8312                            080318                          846 @؉���S1   @؉��o�@7���S���c���`A�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   B   B   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�33B�33B�  B�  B�  B�ffB���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C�fC  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CI�fCL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct�Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#y�D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D)��D*� D+  D+� D,  D,� D-  D-� D.  D.�fD/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DMy�DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dcy�Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Dn��Doy�Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dty�Dy�HD� RD�R�D���D��\D�"�D�O
D���D��D�${D�]qD��RD��HD��D�QHDڣ3D��D��D�[3D�3D�� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @z�G@�
=@�
=A�A;�A[�A{�A�A�A�A�A�A�A�A�B�HB�HB�HB�HB&�HB.�HB6�HB>�HBF�HBN�HBV�HB^�HBf�HBn�HBv�HB~�HB���B���B�p�B�p�B�p�B��
B�=qB�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�=qB�p�C�RC�RC�RC�RC	�RC�RC�RC�RC�RC�RC�RC�RC�RC��C�RC�RC!�RC#�RC%�RC'�RC)�RC+�RC-�RC/�RC1�RC3�RC5�RC7�RC9�RC;�RC=�RC?�RCA�RCC�RCE�RCG�RCI��CK�RCM�RCO�RCQ�RCS�RCU�RCW�RCY�RC[�RC]�RC_�RCa�RCc�RCe�RCg�RCi�RCk�RCm�RCo�RCq�RCs��Cu�RCw�RCy�RC{�RC}�RC�RC��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C���C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C���C���C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)D nD �DnD�DnD�DnD�DtzD�DnD�DnD�DnD�DnD�D	nD	�D
nD
�DnD�DnD�DnD�DnD�DnD�DnD�DnD�DnD�DnD�DnD�DnD�DnD�zDnD�DnD�DnD�DnD�DnD�DnD�DnD�DnD�DnD�D nD �D!nD!�D"nD"�D#g�D#�D$nD$�D%nD%�D&nD&�D'nD'�D(nD(�D)nD)�D*nD*�D+nD+�D,nD,�D-nD-�D.tzD.�D/nD/�D0nD0�D1nD1�D2nD2�D3nD3�D4nD4�D5nD5�D6nD6�D7nD7�D8nD8�D9nD9�D:nD:�D;nD;�D<nD<�D=nD=�D>nD>�D?nD?�D@nD@�DAnDA�DBnDB�DCnDC�DDnDD�DEnDE�DFnDF�DGnDG�DHnDH�DInDI�DJnDJ�DKnDK�DLnDL�DMg�DM�DNnDN�DOnDO�DPnDP�DQnDQ�DRnDR�DSnDS�DTnDT�DUnDU�DVnDV�DWnDW�DXnDX�DYnDY�DZnDZ�D[nD[�D\nD\�D]nD]�D^nD^�D_nD_�D`nD`�DanDa�DbnDb�Dcg�Dc�DdnDd�DenDe�DfnDf�DgnDg�DhnDh�DinDi�DjnDj�DknDk�DlnDl�DmnDm�DnnDn�Dog�Do�DpnDp�DqnDq�DrnDr�DsnDs�Dtg�Dyo\D�\D�I�D���D��fD��D�FD�� D��D��D�T{D��\D��RD��D�HRDښ=D��D��D�R=D�=D��
11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��Aԉ7A�|�AԃAԅAԍPAԋDAԍPAԏ\AԓuAԗ�Aԙ�Aԙ�Aԙ�Aԙ�Aԙ�Aԛ�Aԛ�Aԝ�Aԟ�Aԣ�Aԣ�Aԥ�Aԥ�Aԩ�AԬAԬAԮA԰!A԰!AԮAԬAԩ�Aԟ�A��mAГuA͗�A�;dA�n�A�
=A�/A��A��mA��#A��A�A�ȴA�VA�M�A���A� �A� �A�ȴA��\A��`A��A�(�A�bNA��+A��+A��/A�bNA�l�A�1A��`A���A�I�A�bA���A���A�VA���A�`BA��jA�1'A�
=A���A�E�A��9A���A��RA���A�K�A�K�A�1'A�G�A��A��TA���A��^A��TA�JA�^5A�ȴA�33A��7A��#A��^A���A��hA�(�A���A��uA���A��A�n�A���A�ȴA���A��AK�A}�hA|r�Az�AxbNAw/AuƨAs�PAq�;Aq�An��Al9XAg�mAdn�A`��A^v�A]�^A\�jAZ��AX-AW�7AWAV~�AUƨAT�`AT-ASO�AQ�;AO�;AN1AM&�AL��AK�AKG�AJVAH��AE�;ADffAB��A@��A>-A<ĜA;K�A9ƨA7�PA6�RA5��A5"�A4v�A3;dA2VA1�A/S�A.�A.{A-A-G�A,�uA+|�A+�A*�A*�A);dA(z�A'�A'XA&�/A&z�A%�;A%7LA$VA"��A!��A �A ~�A I�A7LAv�A�AS�AĜA5?A��Av�A|�A�A��A�9A��A��A�#A��A9XA�AoA��AO�AVA��A��A9XA�!A
ĜA
z�A
  A	hsA	S�A��A��AVA`BAA��A1'AVAE�AJA �A I�@��@���@���@�v�@���@�M�@�r�@�t�@@�bN@�^@��m@�5?@䛦@�dZ@��@�Ĝ@�(�@ް!@��@�O�@���@�o@��#@ؼj@�9X@׾w@��y@֗�@Ցh@Լj@��m@Ӯ@��@�v�@���@��H@�/@�1'@���@˅@�M�@��`@Ǯ@ř�@ċD@��@öF@�;d@°!@�p�@��@�b@��@�`B@���@�A�@��@���@�-@�p�@��/@��/@��/@�Ĝ@���@�z�@��H@�7L@�j@�b@�C�@�-@��
@�^5@��@�Ĝ@�t�@���@�^5@���@�z�@��@��y@��@���@���@�ȴ@��\@�^5@��7@���@�z�@�I�@��m@�33@��H@�@��H@��@�M�@�&�@���@�1@�l�@�ȴ@��R@���@�{@�@�V@��@�bN@��
@��w@��;@��m@�1@���@���@��@�|�@�K�@�C�@��@��+@�v�@�M�@��@���@��@��@��@���@���@�hs@�G�@��@��9@��D@��@�j@�(�@� �@��@��P@�K�@��@���@�v�@�^5@�E�@��T@���@���@�?}@��@��`@���@��u@�j@�Q�@�b@��m@��w@�t�@�;d@�
=@�ȴ@��!@�n�@�{@��@���@�&�@�V@��@��/@���@�I�@�Q�@�Q�@� �@�b@���@��m@���@��@��@��!@�v�@�n�@�n�@��+@��@��@�o@���@�ȴ@�E�@���@�x�@��7@���@���@���@��-@�p�@�X@�Ĝ@�1'@��;@��;@���@��w@�\)@�;d@�+@�@���@���@��\@��+@�~�@�v�@�v�@�n�@�n�@�n�@�=q@��@���@��@�/@�Ĝ@�Q�@�b@�  @���@�l�@�33@��@���@��H@��R@�ff@�E�@��@�@�@��@�p�@�`B@�&�@��@��j@���@���@���@��u@��@~��@t�_@lbN@e|@\�I@T$@Nq�@FOv@>��@6��@2�@-��@)�@%�h@![W@�~@��@�/@��@	l11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   Aԉ7A�|�AԃAԅAԍPAԋDAԍPAԏ\AԓuAԗ�Aԙ�Aԙ�Aԙ�Aԙ�Aԙ�Aԛ�Aԛ�Aԝ�Aԟ�Aԣ�Aԣ�Aԥ�Aԥ�Aԩ�AԬAԬAԮA԰!A԰!AԮAԬAԩ�Aԟ�A��mAГuA͗�A�;dA�n�A�
=A�/A��A��mA��#A��A�A�ȴA�VA�M�A���A� �A� �A�ȴA��\A��`A��A�(�A�bNA��+A��+A��/A�bNA�l�A�1A��`A���A�I�A�bA���A���A�VA���A�`BA��jA�1'A�
=A���A�E�A��9A���A��RA���A�K�A�K�A�1'A�G�A��A��TA���A��^A��TA�JA�^5A�ȴA�33A��7A��#A��^A���A��hA�(�A���A��uA���A��A�n�A���A�ȴA���A��AK�A}�hA|r�Az�AxbNAw/AuƨAs�PAq�;Aq�An��Al9XAg�mAdn�A`��A^v�A]�^A\�jAZ��AX-AW�7AWAV~�AUƨAT�`AT-ASO�AQ�;AO�;AN1AM&�AL��AK�AKG�AJVAH��AE�;ADffAB��A@��A>-A<ĜA;K�A9ƨA7�PA6�RA5��A5"�A4v�A3;dA2VA1�A/S�A.�A.{A-A-G�A,�uA+|�A+�A*�A*�A);dA(z�A'�A'XA&�/A&z�A%�;A%7LA$VA"��A!��A �A ~�A I�A7LAv�A�AS�AĜA5?A��Av�A|�A�A��A�9A��A��A�#A��A9XA�AoA��AO�AVA��A��A9XA�!A
ĜA
z�A
  A	hsA	S�A��A��AVA`BAA��A1'AVAE�AJA �A I�@��@���@���@�v�@���@�M�@�r�@�t�@@�bN@�^@��m@�5?@䛦@�dZ@��@�Ĝ@�(�@ް!@��@�O�@���@�o@��#@ؼj@�9X@׾w@��y@֗�@Ցh@Լj@��m@Ӯ@��@�v�@���@��H@�/@�1'@���@˅@�M�@��`@Ǯ@ř�@ċD@��@öF@�;d@°!@�p�@��@�b@��@�`B@���@�A�@��@���@�-@�p�@��/@��/@��/@�Ĝ@���@�z�@��H@�7L@�j@�b@�C�@�-@��
@�^5@��@�Ĝ@�t�@���@�^5@���@�z�@��@��y@��@���@���@�ȴ@��\@�^5@��7@���@�z�@�I�@��m@�33@��H@�@��H@��@�M�@�&�@���@�1@�l�@�ȴ@��R@���@�{@�@�V@��@�bN@��
@��w@��;@��m@�1@���@���@��@�|�@�K�@�C�@��@��+@�v�@�M�@��@���@��@��@��@���@���@�hs@�G�@��@��9@��D@��@�j@�(�@� �@��@��P@�K�@��@���@�v�@�^5@�E�@��T@���@���@�?}@��@��`@���@��u@�j@�Q�@�b@��m@��w@�t�@�;d@�
=@�ȴ@��!@�n�@�{@��@���@�&�@�V@��@��/@���@�I�@�Q�@�Q�@� �@�b@���@��m@���@��@��@��!@�v�@�n�@�n�@��+@��@��@�o@���@�ȴ@�E�@���@�x�@��7@���@���@���@��-@�p�@�X@�Ĝ@�1'@��;@��;@���@��w@�\)@�;d@�+@�@���@���@��\@��+@�~�@�v�@�v�@�n�@�n�@�n�@�=q@��@���@��@�/@�Ĝ@�Q�@�b@�  @���@�l�@�33@��@���@��H@��R@�ff@�E�@��@�@�@��@�p�@�`B@�&�@��@��j@���@���@���G�O�@��@~��@t�_@lbN@e|@\�I@T$@Nq�@FOv@>��@6��@2�@-��@)�@%�h@![W@�~@��@�/@��@	l11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B)�BJ�BZBn�B�B�+B�VB��B��B��B��B��B��B��B��B��B�uB��B�oB�PB�VB�JB�JB�DB�7B�%B�B�B|�B{�By�Bw�Bs�BiyBcTBaHB[#BVBO�BE�BB�B>wB9XB0!B&�B�BB��B�#BŢB�XB�B��B��B�\B�BhsBZBN�BE�B9XB-B�BDB
��B
�B
��B
�RB
�B
�bB
v�B
ZB
VB
M�B
7LB
)�B
�B
VB
%B	�B	�fB	�)B	��B	�wB	�XB	�B	�{B	w�B	[#B	A�B	0!B	(�B	!�B	�B	+B	B	  B��B��B�B�B�yB�HB�B��BŢBB��B�dB�FB�B��B��B�{B�\B�%B�B{�Bu�Bo�Bl�BjBiyBiyBjBiyBn�BjBn�Bo�Br�Bp�Bl�BjBhsBhsBhsBhsBo�Bt�Bu�Bt�Bs�Br�Bo�Bn�BjBhsBffBgmBhsBhsBe`BcTBaHB_;B^5B\)B\)BYBW
BYBW
BR�BN�BQ�BM�BK�BK�BK�BP�BVBXBW
BVBT�BQ�BH�BF�BF�BE�BD�BD�BA�BC�BD�BD�BC�BD�BD�BB�BA�B@�B=qB:^B9XB:^B;dB;dB:^B8RB8RB7LB5?B33B33B6FB8RB:^B=qB=qB?}BB�BC�BE�BG�BJ�BI�BE�BF�BF�BG�BH�BH�BI�BK�BJ�BJ�BL�BO�BR�BR�BQ�BR�BR�BS�BW
BXBZBZB[#B^5B_;B_;BdZBffBgmBhsBhsBjBn�Bp�Br�Bx�By�Bx�Bx�Bx�Bx�Bw�Bw�B|�B|�B{�Bz�Bz�B{�B�B�B�7B�7B�PB�VB�hB��B��B��B��B��B��B��B�!B�3B�9B�RB�^B�jB�qB�wB�}B�}BÖBĜBĜBǮB��B��B��B��B��B��B��B��B��B��B�B�B�#B�BB�ZB�`B�fB�B�B��B��B��B��B��B��B��B��B	B	1B		7B	
=B	bB	uB	�B	�B	�B	�B	!�B	&�B	'�B	,B	/B	/B	/B	1'B	2-B	5?B	9XB	;dB	=qB	@�B	E�B	H�B	I�B	L�B	P�B	P�B	R�B	YB	[#B	\)B	_;B	bNB	cTB	e`B	hsB	jB	k�B	k�B	l�B	p�B	q�B	s�B	y�B	z�B	}�B	� B	�B	�B	�%B	�+B	�7B	�=B	�=B	�=B	�DB	�VB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�'B	�'B	�-B	�-B	�-B	�?B	�FB	�LB	�jB	�wB	��B	��B	B	B	B	B	ÖB	ÖB	B	ŢB	ǮB	ȴB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�)B	�)B	�)B	�/B	�;B	�HB	�NB	�NB	�NB	�NB	�B	�B
�B
\B
�B
 'B
(XB
0!B
:�B
A�B
IB
N�B
Q4B
WYB
Y�B
^B
`�B
e�B
l�B
o�B
r-11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B!$BA�BQCBe�Bz8B~RB�|B��B��B��B��B��B��B��B��B��B��B��B��B�yB�B�tB�tB�nB�aB}PBz=Bx1BtBsBqBn�Bj�B`�BZ�BXvBRRBM3BGB<�B9�B5�B0�B'SBB�B�<B�B�\B��B��B�QB�B��B��ByMB_�BQaBFB<�B0�B$UB�B�B
�'B
��B
�AB
��B
�fB
��B
nB
QvB
M]B
E-B
.�B
!YB
�B
�B	��B	�B	��B	ӌB	�+B	��B	��B	�oB	��B	o:B	R�B	8�B	'�B	 hB	>B	�B��B��B�vB�dB�KB�-B�B��B��BωB�;B�B�
B��B��B��B��B�OB�+B��B��B}�Bx�BsiBmFBg"BdBbB`�B`�BbB`�BfBbBfBg#Bj5Bh)BdBbB_�B_�B_�B_�Bg$BlBBmIBlBBk<Bj6Bg$BfBbB_�B]�B^�B_�B_�B\�BZ�BX�BV�BU�BS�BS�BP�BN�BP�BN�BJ}BFdBIwBE^BCSBCSBCSBHqBM�BO�BN�BM�BL�BIxB@AB>5B>5B=/B<)B<*B9B;$B<*B<*B;$B<*B<+B:B9B8B5 B1�B0�B1�B2�B2�B1�B/�B/�B.�B,�B*�B*�B-�B/�B1�B5B5B7B:!B;(B=4B?@BBSBALB=4B>:B>:B?@B@FB@FBALBCYBBSBBSBD_BGqBJ�BJ�BIBJ�BJ�BK�BN�BO�BQ�BQ�BR�BU�BV�BV�B[�B]�B^�B`B`BbBf*Bh6BjBBpgBqmBpgBpgBpgBpgBoaBoaBt�Bt�BsyBrsBrsBsyBx�B|�B��B��B��B��B��B�B�B�1B�IB�OB�mB��B��B��B��B��B��B��B� B�B�B�B�%B�+B�+B�=B�[B�UB�aB�gB�gB�gB�gB�mB�sB̌B͒BϞBұB��B��B��B��B�B�0B�OB�UB�aB�gB�gB�mB�zB��B��B��B	 �B	�B	�B	
�B	B	*B	6B	<B	TB	rB	yB	#�B	&�B	&�B	&�B	(�B	)�B	,�B	0�B	2�B	4�B	8B	=)B	@;B	AAB	DTB	HlB	HlB	JyB	P�B	R�B	S�B	V�B	Y�B	Z�B	\�B	_�B	bB	c
B	c
B	dB	h)B	i/B	k;B	q_B	reB	uxB	w�B	z�B	|�B	}�B	~�B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�(B	�4B	�4B	�:B	�@B	�@B	�MB	�YB	�eB	�rB	�wB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�	B	�B	�B	�B	�B	�B	�B	�B	�"B	�.B	�4B	�4B	�4B	�:B	�AB	�AB	�RB	�^B	�dB	�kB	�kB	�kB	�qB	�wB	̓B	ΉB	ϏB	ЖB	ќB	ӨB	ӨB	ӨB	ԮB	ֺB	��B	��B	��B	��G�O�B	��B	�B	�B
�B
RB
�B
�B
'�B
2B
99B
@�B
FB
H�B
N�B
QHB
UzB
XXB
]wB
dB
g1B
i�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.28 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9998(+/-0.0001), vertically averaged dS =-0.008(+/-0.002) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144122022020411441220220204114412  AO  ARCAADJP                                                                    20200619170857    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200619170857  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200619170857  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114412  IP                  G�O�G�O�G�O�                