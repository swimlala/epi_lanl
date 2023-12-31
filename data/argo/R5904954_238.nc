CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:17:43Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7(   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7h   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8,   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           80   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    88   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8<   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8D   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8L   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8T   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8X   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8`   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9`   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9d   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9h   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9l   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  A<   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C0   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  T�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ^|   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  fH   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h<   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  p   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  w�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  y�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �T   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �    HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �$   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �(   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �,   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �0Argo profile    3.1 1.2 19500101000000  20181005191743  20181005191743  5904954 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6557                            2B  A   APEX                            7468                            062512                          846 @���Յ�1   @���`�@4���"���d���+1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @@  @�33@�  A   AffA@  A`  A�  A�  A�  A�  A�  A�  A�  A�33B   B  B  B  B   B(  B0  B8  B?��BH  BP  BX  B`  Bh  Bp  Bx  B��B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B���B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C)�fC,  C.�C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CY�fC\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn�Cp�Cr  Cs�fCu�fCw�fCy�fC{�fC~  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C��3C��C��C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��C�  C�  C��C�  C��C��C�  C�  C�  C�  C�  C��3C��C�  C�  C�  C�  C��C�  C�  C��C��C�  C��3C�  C��C�  C�  C��C�  C�  C��3C�  C��C��C��C��C��C�  C�  C�  C�  C��C��C�  C�  C��C��C�  C�  C��C�  C�  C�  C�  C�  C��C��C��C��3C��3C��3C�  C�  C�  C��3C��3C�  C��3C��3C�  C��C�  C��3C��C��3C�  C��C�  C��3C��C�  C��3C��3C��3C��3C�  D fD �fD ��D� D  D� D  D�fD��D� D��Dy�D��D�fD  D� D  Dy�D	fD	�fD
fD
y�D  D� D  D�fD  Dy�DfD� D��D� D  D�fD  Dy�D��D� DfDy�D  D� D  D� D  Dy�D  D�fDfD� DfD� D��D� D  D� D  Dy�D�3D� D�D� D��D� D �D �fD ��D!�fD!��D"� D#fD#� D$  D$� D%fD%y�D&  D&y�D'fD'y�D(  D(�fD(��D)� D*  D*� D*��D+� D,  D,� D-fD-� D-��D.� D/  D/� D0  D0y�D1  D1� D2  D2� D3  D3� D3��D4� D5  D5� D6  D6�fD7fD7y�D7��D8� D9  D9� D:fD:�fD:��D;�fD<  D<� D=  D=� D>  D>�fD>��D?y�D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DEfDEy�DFfDFy�DGfDG� DH  DH�fDH��DI�fDJ  DJ� DK  DK� DK��DLy�DM  DM�fDNfDN� DO  DOy�DO��DPy�DP��DQy�DRfDR� DSfDS� DT  DTy�DU  DUy�DV  DV� DV��DW�fDW��DXy�DY  DYy�DZfDZ� D[fD[�fD\  D\� D]fD]y�D^fD^�fD_  D_y�D`fD`y�Da  Da�fDa��Dby�DcfDc� Dd  Dd� De  De� Df  Df� DgfDg� Dh  Dh� DifDi� Dj  Dj� Dj��Dk� DlfDly�Dm  Dm� DnfDny�DofDoy�Dp  Dpy�Dq  Dq�fDr  Dr� Ds  Ds� Dt  Dt�fDu  Du� Dv  Dv� Dw  Dwy�Dw�3Dy�HD�Q�D�Q�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @P  @�33@�  A  A"ffAD  Ad  A�  A�  A�  A�  A�  A�  A�  A�33B  B	  B  B  B!  B)  B1  B9  B@��BI  BQ  BY  Ba  Bi  Bq  By  B�L�B�� B��3B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� B��3B�� B�� BĀ BȀ B̀ BЀ BԀ B؀ B܀ B�� B�L�B�L�B� B�� B� B�� B�� C @ C@ C@ C@ C@ C
@ C@ C@ C@ C@ C@ C@ C@ C@ C@ C@ C @ C"@ C$@ C&@ C(@ C*&fC,@ C.Y�C0@ C2@ C4@ C6@ C8@ C:@ C<@ C>@ C@@ CB@ CD@ CF@ CH@ CJ@ CL@ CN@ CP@ CR@ CT@ CV@ CX@ CZ&fC\@ C^@ C`@ Cb@ Cd@ Cf@ Ch@ Cj@ Cl@ CnY�CpY�Cr@ Ct&fCv&fCx&fCz&fC|&fC~@ C�  C�  C�  C�  C�  C�  C�  C�  C�3C�  C�  C�  C�  C�  C�  C�  C�  C�,�C�  C�3C�,�C�,�C�  C�  C�  C�  C�3C�3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�,�C�  C�  C�,�C�  C�  C�,�C�  C�,�C�,�C�  C�  C�  C�  C�  C�3C�,�C�  C�  C�  C�  C�,�C�  C�  C�,�C�,�C�  C�3C�  C�,�C�  C�  C�,�C�  C�  C�3C�  C�,�C�,�C�,�C�,�C�,�C�  C�  C�  C�  C�,�C�,�C�  C�  C�,�C�,�C�  C�  C�,�C�  C�  C�  C�  C�  C�,�C�,�C�,�C�3C�3C�3C�  C�  C�  C�3C�3C�  C�3C�3C�  C�,�C�  C�3C�,�C�3C�  C�,�C�  C�3C�,�C�  C�3C�3C�3C�3C�  D fD �fD	�D� D D� D D�fD	�D� D	�D��D	�D�fD D� D D��D	fD	�fD
fD
��D D� D D�fD D��DfD� D	�D� D D�fD D��D	�D� DfD��D D� D D� D D��D D�fDfD� DfD� D	�D� D D� D D��D3D� D�D� D	�D� D �D �fD!	�D!�fD"	�D"� D#fD#� D$ D$� D%fD%��D& D&��D'fD'��D( D(�fD)	�D)� D* D*� D+	�D+� D, D,� D-fD-� D.	�D.� D/ D/� D0 D0��D1 D1� D2 D2� D3 D3� D4	�D4� D5 D5� D6 D6�fD7fD7��D8	�D8� D9 D9� D:fD:�fD;	�D;�fD< D<� D= D=� D> D>�fD?	�D?��D@ D@� DA DA� DB DB� DC DC� DD DD� DEfDE��DFfDF��DGfDG� DH DH�fDI	�DI�fDJ DJ� DK DK� DL	�DL��DM DM�fDNfDN� DO DO��DP	�DP��DQ	�DQ��DRfDR� DSfDS� DT DT��DU DU��DV DV� DW	�DW�fDX	�DX��DY DY��DZfDZ� D[fD[�fD\ D\� D]fD]��D^fD^�fD_ D_��D`fD`��Da Da�fDb	�Db��DcfDc� Dd Dd� De De� Df Df� DgfDg� Dh Dh� DifDi� Dj Dj� Dk	�Dk� DlfDl��Dm Dm� DnfDn��DofDo��Dp Dp��Dq Dq�fDr Dr� Ds Ds� Dt Dt�fDu Du� Dv Dv� Dw Dw��Dw�3Dy�HD�Y�D�Y�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�ƨA���A�ȴA�ƨA���A���A���A���A���A�ĜAȧ�AȅA�A�A�ƨA��TA�jA�`BA�=qA�"�A��A�
=A�A���A��A��mA��/A���AŮAŗ�A�G�A���A��/A���AĶFAď\A�x�A�E�A��A���A�E�A²-A��
A�XA�XA�ZA�ZA�XA��A�bA�
=A�ĜA�z�A�hsA�7LA�bA�|�A�  A��A�C�A��mA��RA���A�S�A�M�A�ȴA�ƨA���A�oA���A��PA��A�(�A���A�
=A�n�A���A��RA���A��^A�(�A���A��TA�XA���A�x�A���A�|�A�"�A��A�G�A�|�A�;dA�/A���A�/A���A���A���A��+A���A���A�r�A�JA��^A�JA�1'A��A�VA�ffA��A}7LAzȴAz�Ay�Ax��Ax�Axz�Au�^AtArE�AqC�Ao�mAn��Al�\Aj��Ag`BAb(�A_�A_
=A^�!A\^5AZ��AX��AVM�ATĜAR^5AO��AOVAM�AL�DAJ��AH��AGAF�\AF �AD�AC&�AA��A@��A@=qA?&�A=x�A="�A<�A;�7A9C�A8�A7C�A5�A4�uA3��A3A1�FA/�TA.(�A,n�A+A*=qA(��A(��A&�`A$JA"VA"1A!p�A�-A/Al�A-A�`A�-A?}A�\A�uA1AC�A�A�TA�A^5Ax�A  A&�A��A
I�A	`BA	"�A9XA�PA�RA��Ap�A�9A��A
=A~�A-AƨA �/@�?}@�ƨ@���@�7L@��P@��!@�x�@���@�^5@�/@�
=@�p�@��@���@�9X@�33@��#@��;@�\)@�C�@�+@�5?@��@㕁@�^@��
@�33@��@��@�Q�@���@�J@ؓu@�ȴ@��#@�Q�@�S�@�33@�+@�n�@�/@��m@��@��@̴9@�o@� �@�{@�j@å�@�
=@�E�@��T@��@�A�@��F@���@�dZ@��y@���@�~�@�@��@�&�@�/@�1@���@��`@�Z@�1@�;d@���@��T@�X@�/@���@�z�@��@��@�I�@�1'@�1'@��@��m@��F@���@��@�ȴ@���@�V@�E�@�V@��+@�v�@�J@��#@���@���@��@�{@��@�5?@�~�@�n�@�^5@�J@�G�@��`@��D@�r�@�Q�@�  @��P@�S�@��@���@���@��7@�x�@�O�@���@�bN@��m@�t�@�K�@�n�@���@���@�`B@�?}@��`@�  @���@�;d@�ff@��^@�hs@���@��@�Ĝ@�z�@��;@�n�@��@��u@��
@�z�@��/@�/@��-@�J@�@���@�O�@�A�@��w@���@���@�v�@���@��9@��j@��j@�1'@���@�+@��@�ff@�M�@���@��-@��/@�Z@���@��!@�n�@���@�~�@��@���@���@���@��T@��@��@��T@�@�X@�V@��j@���@�j@�Z@�I�@�1'@���@�A�@�  @�1'@�r�@�l�@�
=@���@��@��@���@���@��y@���@�~�@�M�@�$�@�ff@�~�@�$�@��/@�(�@��
@���@�t�@�K�@�@��y@��H@��H@��@���@��R@�~�@�E�@���@�/@���@���@��D@�b@��@��w@�|�@�@��\@��+@��H@���@���@�ȴ@��R@�v�@��@�X@�%@��j@�  @��w@��w@��@�t�@�l�@�dZ@�K�@�o@�ȴ@���@��\@�~�@�v�@�E�@�$�@��#@�hs@�?}@�/@���@�Ĝ@��D@� �@�(�@��@���@�S�@�"�@�ȴ@�-@���@��-@���@��#@���@p�v@^O1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�ƨA���A�ȴA�ƨA���A���A���A���A���A�ĜAȧ�AȅA�A�A�ƨA��TA�jA�`BA�=qA�"�A��A�
=A�A���A��A��mA��/A���AŮAŗ�A�G�A���A��/A���AĶFAď\A�x�A�E�A��A���A�E�A²-A��
A�XA�XA�ZA�ZA�XA��A�bA�
=A�ĜA�z�A�hsA�7LA�bA�|�A�  A��A�C�A��mA��RA���A�S�A�M�A�ȴA�ƨA���A�oA���A��PA��A�(�A���A�
=A�n�A���A��RA���A��^A�(�A���A��TA�XA���A�x�A���A�|�A�"�A��A�G�A�|�A�;dA�/A���A�/A���A���A���A��+A���A���A�r�A�JA��^A�JA�1'A��A�VA�ffA��A}7LAzȴAz�Ay�Ax��Ax�Axz�Au�^AtArE�AqC�Ao�mAn��Al�\Aj��Ag`BAb(�A_�A_
=A^�!A\^5AZ��AX��AVM�ATĜAR^5AO��AOVAM�AL�DAJ��AH��AGAF�\AF �AD�AC&�AA��A@��A@=qA?&�A=x�A="�A<�A;�7A9C�A8�A7C�A5�A4�uA3��A3A1�FA/�TA.(�A,n�A+A*=qA(��A(��A&�`A$JA"VA"1A!p�A�-A/Al�A-A�`A�-A?}A�\A�uA1AC�A�A�TA�A^5Ax�A  A&�A��A
I�A	`BA	"�A9XA�PA�RA��Ap�A�9A��A
=A~�A-AƨA �/@�?}@�ƨ@���@�7L@��P@��!@�x�@���@�^5@�/@�
=@�p�@��@���@�9X@�33@��#@��;@�\)@�C�@�+@�5?@��@㕁@�^@��
@�33@��@��@�Q�@���@�J@ؓu@�ȴ@��#@�Q�@�S�@�33@�+@�n�@�/@��m@��@��@̴9@�o@� �@�{@�j@å�@�
=@�E�@��T@��@�A�@��F@���@�dZ@��y@���@�~�@�@��@�&�@�/@�1@���@��`@�Z@�1@�;d@���@��T@�X@�/@���@�z�@��@��@�I�@�1'@�1'@��@��m@��F@���@��@�ȴ@���@�V@�E�@�V@��+@�v�@�J@��#@���@���@��@�{@��@�5?@�~�@�n�@�^5@�J@�G�@��`@��D@�r�@�Q�@�  @��P@�S�@��@���@���@��7@�x�@�O�@���@�bN@��m@�t�@�K�@�n�@���@���@�`B@�?}@��`@�  @���@�;d@�ff@��^@�hs@���@��@�Ĝ@�z�@��;@�n�@��@��u@��
@�z�@��/@�/@��-@�J@�@���@�O�@�A�@��w@���@���@�v�@���@��9@��j@��j@�1'@���@�+@��@�ff@�M�@���@��-@��/@�Z@���@��!@�n�@���@�~�@��@���@���@���@��T@��@��@��T@�@�X@�V@��j@���@�j@�Z@�I�@�1'@���@�A�@�  @�1'@�r�@�l�@�
=@���@��@��@���@���@��y@���@�~�@�M�@�$�@�ff@�~�@�$�@��/@�(�@��
@���@�t�@�K�@�@��y@��H@��H@��@���@��R@�~�@�E�@���@�/@���@���@��D@�b@��@��w@�|�@�@��\@��+@��H@���@���@�ȴ@��R@�v�@��@�X@�%@��j@�  @��w@��w@��@�t�@�l�@�dZ@�K�@�o@�ȴ@���@��\@�~�@�v�@�E�@�$�@��#@�hs@�?}@�/@���@�Ĝ@��D@� �@�(�@��@���@�S�@�"�@�ȴ@�-@���@��-@���@��#@���@p�v@^O1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�B�B�
B�
B�
B�B�#B�5B�`B�B��B��B��B+B�B)�B(�B'�B)�B1'B5?B6FB6FB8RB;dB=qB@�BE�BE�BF�BK�BN�BN�BP�BT�BW
B\)B`BBbNB|�B�VB�B�#B�BB�BB�HB�TB�B�B�B��B��B��B��B�B�3B��B��B��B��B��B�uB�hB�\B�JB�1B�Bu�Bu�By�Bt�BgmBYB_;B^5B\)BVBI�B.B�BPB  B�B��BǮB��B�XB�?B�!B��B�JB�B{�Bt�Bn�BffBZBD�B0!BuB
��B
��B
��B
ŢB
�3B
�hB
iyB
S�B
D�B
6FB
%�B
7LB
?}B
@�B
M�B
R�B
R�B
D�B
=qB
49B
-B
!�B
�B
1B	��B	�BB	�jB	�-B	�B	��B	��B	�PB	� B	o�B	dZB	VB	I�B	C�B	<jB	7LB	.B	#�B	�B	�B	�B	hB	
=B	B��B��B�B�B�yB�fB�TB�#B�
B��B��B��B��B��B��B��BȴBĜBƨBÖB��B�qB�XB�3B�B�B�B��B��B��B��B�uB�bB�VB�VB�+B�%B�=B�PB�JB�7B�+B�B�B�B�B� B� B~�B}�B}�B|�B|�B{�B|�B{�Bz�By�By�Bx�Bw�By�By�Bx�By�Bz�By�Bx�By�By�By�B{�B� B�B�B�B�B�B�B�B�B�B�%B�1B�7B�7B�7B�1B�+B�%B�%B�PB�VB�uB�{B�{B��B��B��B��B��B�oB�oB�oB�oB�hB�PB�1B�=B�JB�VB�\B�bB�hB�{B��B��B��B��B��B��B��B�B�3B�FB�RB�XB�XB�RB�RB�LB�RB�RB�XB�jBBƨBȴB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�
B�5B�NB�`B�B�B�B��B��B	B	+B		7B	\B	{B	�B	�B	�B	�B	�B	"�B	#�B	'�B	)�B	)�B	,B	,B	-B	1'B	33B	8RB	<jB	=qB	A�B	A�B	D�B	F�B	I�B	M�B	R�B	S�B	VB	XB	YB	YB	XB	]/B	_;B	_;B	^5B	[#B	YB	W
B	VB	\)B	cTB	gmB	k�B	p�B	v�B	z�B	z�B	z�B	y�B	x�B	x�B	y�B	|�B	� B	�B	�B	�B	�B	�B	�B	�B	�%B	�B	�%B	�+B	�1B	�+B	�%B	�%B	�1B	�7B	�7B	�7B	�=B	�JB	�\B	�bB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�9B	�9B	�9B	�9B	�?B	�RB	�dB	�^B	�RB	�^B	�qB	�}B	��B	��B	ĜB	ŢB	ŢB	ŢB	ƨB	ƨB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�5B	�BB	�HB	�NB	�ZB	�`B	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
�B
,�2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 B�B�B�
B�
B�
B�B�#B�5B�`B�B��B��B��B+B�B)�B(�B'�B)�B1'B5?B6FB6FB8RB;dB=qB@�BE�BE�BF�BK�BN�BN�BP�BT�BW
B\)B`BBbNB|�B�VB�B�#B�BB�BB�HB�TB�B�B�B��B��B��B��B�B�3B��B��B��B��B��B�uB�hB�\B�JB�1B�Bu�Bu�By�Bt�BgmBYB_;B^5B\)BVBI�B.B�BPB  B�B��BǮB��B�XB�?B�!B��B�JB�B{�Bt�Bn�BffBZBD�B0!BuB
��B
��B
��B
ŢB
�3B
�hB
iyB
S�B
D�B
6FB
%�B
7LB
?}B
@�B
M�B
R�B
R�B
D�B
=qB
49B
-B
!�B
�B
1B	��B	�BB	�jB	�-B	�B	��B	��B	�PB	� B	o�B	dZB	VB	I�B	C�B	<jB	7LB	.B	#�B	�B	�B	�B	hB	
=B	B��B��B�B�B�yB�fB�TB�#B�
B��B��B��B��B��B��B��BȴBĜBƨBÖB��B�qB�XB�3B�B�B�B��B��B��B��B�uB�bB�VB�VB�+B�%B�=B�PB�JB�7B�+B�B�B�B�B� B� B~�B}�B}�B|�B|�B{�B|�B{�Bz�By�By�Bx�Bw�By�By�Bx�By�Bz�By�Bx�By�By�By�B{�B� B�B�B�B�B�B�B�B�B�B�%B�1B�7B�7B�7B�1B�+B�%B�%B�PB�VB�uB�{B�{B��B��B��B��B��B�oB�oB�oB�oB�hB�PB�1B�=B�JB�VB�\B�bB�hB�{B��B��B��B��B��B��B��B�B�3B�FB�RB�XB�XB�RB�RB�LB�RB�RB�XB�jBBƨBȴB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�
B�5B�NB�`B�B�B�B��B��B	B	+B		7B	\B	{B	�B	�B	�B	�B	�B	"�B	#�B	'�B	)�B	)�B	,B	,B	-B	1'B	33B	8RB	<jB	=qB	A�B	A�B	D�B	F�B	I�B	M�B	R�B	S�B	VB	XB	YB	YB	XB	]/B	_;B	_;B	^5B	[#B	YB	W
B	VB	\)B	cTB	gmB	k�B	p�B	v�B	z�B	z�B	z�B	y�B	x�B	x�B	y�B	|�B	� B	�B	�B	�B	�B	�B	�B	�B	�%B	�B	�%B	�+B	�1B	�+B	�%B	�%B	�1B	�7B	�7B	�7B	�=B	�JB	�\B	�bB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�9B	�9B	�9B	�9B	�?B	�RB	�dB	�^B	�RB	�^B	�qB	�}B	��B	��B	ĜB	ŢB	ŢB	ŢB	ƨB	ƨB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�5B	�BB	�HB	�NB	�ZB	�`B	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
�B
,�2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.25 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005191743                              AO  ARCAADJP                                                                    20181005191743    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005191743  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005191743  QCF$                G�O�G�O�G�O�8000            