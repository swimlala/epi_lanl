CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:17:33Z creation      
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
resolution        =���   axis      Z        l  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  @�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     l  B�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J$   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     l  L    TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  Sl   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Z�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  \�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  d    TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  e�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  mh   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  t�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  v�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ~   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �d   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �    HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �$   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �4   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �8   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �<   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �@Argo profile    3.1 1.2 19500101000000  20181005191733  20181005191733  5904954 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6557                            2B  A   APEX                            7468                            062512                          846 @��$���1   @��%��k�@6^��"���d���R1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   B   B   @333@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bo��Bx  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C�C�C  C�fC
  C  C  C  C  C  C  C  C  C�fC�fC   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8�C:�C<  C>  C@  CB  CD  CF  CH  CJ  CL  CM�fCO�fCR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd�Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cw�fCy�fC|  C~  C�  C�  C��3C��3C��3C��3C��3C�  C�  C��C��C��C��C�  C��C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C��fC�  C��C��C�  C�  C��3C�  C�  C��3C�  C�  C�  C��C��C��3C��C�  C��C��C�  C��3C��C��3C��fC�  C��C��3C��3C�  C�  C�  C��C��C��C��C�  C�  C��C�  C��C��C�  C�  C��C��C��C��C��C��3C��3C��3C��3C�  C�  C�  C�  C�  C��C�  C��3C�  C�  C�  C��3C��3C��3C��3C�  C��C��C�  C�  C�  C��C�  C��3C��3D � DfD� D  Dy�D��D� DfD�fDfD�fD  D� D  D�fDfD� D��D	y�D
  D
� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D��Dy�D  D� DfD� D  D� D  D� D  D�fD  D� D  D� DfD�fD  D� D  D�fD  Dy�D��D� D��D� D   D y�D!  D!� D"  D"� D#  D#� D#��D$y�D%  D%�fD&fD&� D'  D'y�D'��D(� D)  D)y�D)��D*y�D+  D+� D,  D,� D,��D-� D.fD.��D/fD/�fD0  D0y�D1  D1� D1��D2�fD3  D3� D4fD4� D5  D5� D6  D6� D7  D7� D7��D8� D9fD9� D:  D:� D;  D;y�D;��D<y�D<��D=y�D>  D>� D?  D?y�D@  D@� DAfDA� DA��DBy�DC  DC� DD  DD�fDE  DE� DE��DF� DGfDG� DH  DH� DI  DI� DJfDJ�fDK  DKy�DL  DL�fDM  DM� DNfDN� DO  DO� DP  DP� DQ  DQ�fDRfDR� DS  DS� DS��DTy�DU  DU�fDVfDV� DW  DWy�DX  DX� DX��DYy�DZ  DZ� DZ��D[y�D\  D\y�D\��D]y�D]��D^s3D^��D_� D`  D`y�Da  Da� Da��Db� Dc  Dc� DdfDd� De  De�fDf  Dfy�Df��Dg�fDh  Dhy�DifDi� Di��Dj� Dj��Dk� DlfDly�Dm  Dm� Dn  Dn�fDo  Doy�Dp  Dpy�Dp��Dq� Dq��Dr� Ds  Ds� Ds��Dts3Du  Du� Dv  Dv� Dw  Dwy�Dw�fDy�qD�D{1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @0��@}p�@��R@��RA\)A?\)A_\)A\)A��A��A��A��AϮA߮A�A��B�
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
Bop�Bw�
B�
B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��BǸRB��B��B��B��B��B��B��B��B��B��B��B��B��B��C]C]C��C�)C	��C��C��C��C��C��C��C��C��C�)C�)C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C8]C:]C;��C=��C?��CA��CC��CE��CG��CI��CK��CM�)CO�)CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cd]Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw�)Cy�)C{��C}��C��C���C��C��C��C��C��C���C���C��C��C��C��C���C��C���C��C���C���C���C���C���C���C���C���C��C���C���C��GC���C��C��C���C���C��C���C���C��C���C���C���C��C��C��C��C���C��C��C���C��C��C��C��GC���C��C��C��C���C���C���C�{C��C��C��C���C���C��C���C��C��C���C���C��C��C�{C��C��C��C��C��C��C���C���C���C���C���C��C���C��C���C���C���C��C��C��C��C���C��C��C���C���C���C��C���C��C��D }qD�D}qD�qDwD�D}qD�D��D�D��D�qD}qD�qD��D�D}qD�D	wD	�qD
}qD
�qD}qD�qDwD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�DwD�qD}qD�D}qD�qD}qD�qD}qD�qD��D�qD}qD�qD}qD�D��D�qD}qD�qD��D�qDwD�D}qD�D}qD�qD wD �qD!}qD!�qD"}qD"�qD#}qD#�D$wD$�qD%��D&�D&}qD&�qD'wD'�D(}qD(�qD)wD)�D*wD*�qD+}qD+�qD,}qD,�D-}qD.�D.�>D/�D/��D/�qD0wD0�qD1}qD1�D2��D2�qD3}qD4�D4}qD4�qD5}qD5�qD6}qD6�qD7}qD7�D8}qD9�D9}qD9�qD:}qD:�qD;wD;�D<wD<�D=wD=�qD>}qD>�qD?wD?�qD@}qDA�DA}qDA�DBwDB�qDC}qDC�qDD��DD�qDE}qDE�DF}qDG�DG}qDG�qDH}qDH�qDI}qDJ�DJ��DJ�qDKwDK�qDL��DL�qDM}qDN�DN}qDN�qDO}qDO�qDP}qDP�qDQ��DR�DR}qDR�qDS}qDS�DTwDT�qDU��DV�DV}qDV�qDWwDW�qDX}qDX�DYwDY�qDZ}qDZ�D[wD[�qD\wD\�D]wD]�D^p�D^�D_}qD_�qD`wD`�qDa}qDa�Db}qDb�qDc}qDd�Dd}qDd�qDe��De�qDfwDf�Dg��Dg�qDhwDi�Di}qDi�Dj}qDj�Dk}qDl�DlwDl�qDm}qDm�qDn��Dn�qDowDo�qDpwDp�Dq}qDq�Dr}qDr�qDs}qDs�Dtp�Dt�qDu}qDu�qDv}qDv�qDwwDw��Dy��D�C31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�M�A�M�A�K�A�K�A�E�A�5?A�&�A��A�1A�  A���A���A���A��A��A��A��A��A��mA��`A��TA��;A��;A��/A��;A��/A��#A��A���A���A���A���A���A���A�ȴA�ȴA�ƨA�ƨA�A���Aκ^AΣ�A�1'AͶFA��A�^5A˸RA�A� �Aɛ�A�5?A�AȲ-Aȴ9AȶFAȼjAȾwA���Aȴ9A�|�AǕ�AœuA�A�&�A��RA�+A�|�A�S�A�XA�l�A���A�M�A�A�A�Q�A�bNA���A�I�A�v�A�{A�bA���A���A��/A��`A�7LA��#A�?}A�1'A��A�ƨA��9A�A��9A���A��;A�?}A�r�A�ffA��FA�XA���A�G�A�A��DA�K�A�-A��`A���A�O�A�p�A��A�JA���A���A���A���A�~�A��A�M�A��\A�C�A�x�A�1A�{A�=qA�ĜA��
A�=qA�1'A��A�A�hsA���A���A��wA�?}A��A}t�A|�\Az��AwO�Au��At�jAs��Ao��AnbADn�AC�#AC?}AB�ABbNAAC�A@�\A@n�A?�A=�A=�PA=p�A=�A<bA;VA9��A8 �A5�
A4�A3��A2jA0E�A.bA-��A,r�A+l�A*ZA)7LA((�A%x�A%�A#�
A#"�A"I�A!��A ĜA ^5A��A?}AȴA�A�A��A��A��A��A�-A�Ar�A|�A=qA�A\)AE�AA�A{A�FAbNA�\A�A
~�A
�A	ƨA�\At�A�9A  At�AM�AdZA�A 5?@���@���@���@���@���@��@��D@��@�|�@���@�ƨ@�+@��@�@�?}@�9@�z�@�I�@���@��@�5?@���@��`@���@�r�@��@�-@�7L@�1@��@�O�@ە�@��#@�`B@���@և+@�M�@�Z@�dZ@���@Ο�@�^5@�@�O�@�C�@ʗ�@�v�@�ff@��@ȣ�@�  @ǥ�@��@���@��@���@��@��@°!@�x�@��`@��@�bN@�9X@��@��@��@�E�@�@��@�@�-@�5?@��-@�%@���@�I�@�b@��@���@��@�O�@�V@�bN@���@��@�t�@�dZ@��@��y@��@���@�p�@�O�@�?}@���@�(�@��F@�l�@��H@���@�=q@���@� �@�l�@�C�@��@�33@�J@���@���@�b@���@�A�@���@��T@�V@�r�@��@�\)@��y@���@�v�@�^5@�-@�$�@��@��h@��@�Q�@�1@�1@��
@�|�@��y@�n�@�M�@���@��#@��^@��-@�hs@�/@��@��@��9@�Z@� �@��F@��@�+@���@���@�ff@�-@�@��^@��@�X@��@�V@���@��@�9X@��
@�K�@���@�~�@�V@�J@��-@�X@��@���@�z�@��@�  @���@���@�|�@�C�@��H@���@�~�@�v�@�ff@�M�@�J@�x�@�x�@�hs@�p�@��h@��7@�G�@��j@��D@�r�@�bN@�Q�@�I�@�A�@�9X@�9X@�(�@��@�b@���@���@�l�@�l�@�K�@���@���@�v�@�V@�-@�@��T@���@���@�`B@��@��@�r�@�(�@��@���@��@��P@�\)@�
=@�n�@�@���@���@��@�O�@�7L@�V@��j@���@�A�@�ƨ@���@�|�@�t�@�t�@�dZ@�"�@�
=@���@���@��@��!@�1@�61111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�M�A�M�A�K�A�K�A�E�A�5?A�&�A��A�1A�  A���A���A���A��A��A��A��A��A��mA��`A��TA��;A��;A��/A��;A��/A��#A��A���A���A���A���A���A���A�ȴA�ȴA�ƨA�ƨA�A���Aκ^AΣ�A�1'AͶFA��A�^5A˸RA�A� �Aɛ�A�5?A�AȲ-Aȴ9AȶFAȼjAȾwA���Aȴ9A�|�AǕ�AœuA�A�&�A��RA�+A�|�A�S�A�XA�l�A���A�M�A�A�A�Q�A�bNA���A�I�A�v�A�{A�bA���A���A��/A��`A�7LA��#A�?}A�1'A��A�ƨA��9A�A��9A���A��;A�?}A�r�A�ffA��FA�XA���A�G�A�A��DA�K�A�-A��`A���A�O�A�p�A��A�JA���A���A���A���A�~�A��A�M�A��\A�C�A�x�A�1A�{A�=qA�ĜA��
A�=qA�1'A��A�A�hsA���A���A��wA�?}A��A}t�A|�\Az��AwO�Au��At�jAs��Ao��AnbADn�AC�#AC?}AB�ABbNAAC�A@�\A@n�A?�A=�A=�PA=p�A=�A<bA;VA9��A8 �A5�
A4�A3��A2jA0E�A.bA-��A,r�A+l�A*ZA)7LA((�A%x�A%�A#�
A#"�A"I�A!��A ĜA ^5A��A?}AȴA�A�A��A��A��A��A�-A�Ar�A|�A=qA�A\)AE�AA�A{A�FAbNA�\A�A
~�A
�A	ƨA�\At�A�9A  At�AM�AdZA�A 5?@���@���@���@���@���@��@��D@��@�|�@���@�ƨ@�+@��@�@�?}@�9@�z�@�I�@���@��@�5?@���@��`@���@�r�@��@�-@�7L@�1@��@�O�@ە�@��#@�`B@���@և+@�M�@�Z@�dZ@���@Ο�@�^5@�@�O�@�C�@ʗ�@�v�@�ff@��@ȣ�@�  @ǥ�@��@���@��@���@��@��@°!@�x�@��`@��@�bN@�9X@��@��@��@�E�@�@��@�@�-@�5?@��-@�%@���@�I�@�b@��@���@��@�O�@�V@�bN@���@��@�t�@�dZ@��@��y@��@���@�p�@�O�@�?}@���@�(�@��F@�l�@��H@���@�=q@���@� �@�l�@�C�@��@�33@�J@���@���@�b@���@�A�@���@��T@�V@�r�@��@�\)@��y@���@�v�@�^5@�-@�$�@��@��h@��@�Q�@�1@�1@��
@�|�@��y@�n�@�M�@���@��#@��^@��-@�hs@�/@��@��@��9@�Z@� �@��F@��@�+@���@���@�ff@�-@�@��^@��@�X@��@�V@���@��@�9X@��
@�K�@���@�~�@�V@�J@��-@�X@��@���@�z�@��@�  @���@���@�|�@�C�@��H@���@�~�@�v�@�ff@�M�@�J@�x�@�x�@�hs@�p�@��h@��7@�G�@��j@��D@�r�@�bN@�Q�@�I�@�A�@�9X@�9X@�(�@��@�b@���@���@�l�@�l�@�K�@���@���@�v�@�V@�-@�@��T@���@���@�`B@��@��@�r�@�(�@��@���@��@��P@�\)@�
=@�n�@�@���@���@��@�O�@�7L@�V@��j@���@�A�@�ƨ@���@�|�@�t�@�t�@�dZ@�"�@�
=@���@���@��@��!@�1@�61111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B��B��B��B��B��B�!B�?B�XB�}BBÖBÖBĜBŢBŢBƨBƨBǮBȴBȴBɺB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�/B�;B�B+B�B;dB\)Bn�B��BɺB��B�)B�HB�TB�ZB�fB�mB�B��BB��B�fB��B�B�fB�B�NB�qBǮB��B��BǮB�qB��B��B��B�B�RB�9B�B��B��B�\B�B}�B|�Bw�Bl�BcTB]/BP�BG�B>wB@�BB�B<jB'�B�BJB  B��B�B�5B��B��BȴBÖB�9B��B�DB�%B�B� Bt�BjBffBcTB\)BH�B=qB7LB'�BB
�yB
�/B
�
B
��B
��B
��B
�}B
�^B
�3B
��B
��B
�VB
�1B
�B
t�B
l�B
^5B
I�B
=qB
5?B
,B
oB	hsB��B��B��B��B��B��B	B	B	  B��B��B��B��B�B�B�`B�;B�B��B��BɺBĜB�qB�^B�FB�-B�B�B��B��B��B��B��B��B��B�{B�uB�oB�bB�\B�VB�JB�=B�1B�B�B~�Bz�Bu�Bq�Bo�Bk�BiyBffBffBdZBbNB_;B\)BW
BVBT�BR�BP�BN�BM�BQ�BS�BS�BS�BQ�BQ�BL�BL�BL�BJ�BI�BI�BH�BH�BF�BE�BC�BC�BC�BE�BG�BI�BI�BJ�BM�BO�BQ�BR�BR�BR�BQ�BO�BP�BO�BM�BO�BO�BN�BO�BQ�BP�BN�BM�BR�BW
BXBYBZB\)B_;BjBn�Bp�Bp�Bp�Bt�Bx�B~�B�B�B�1B�7B�=B�JB�oB��B��B��B��B��B�B�B�3B�^B�qB�}B��B��BĜBɺB��B��B��B��B��B�B�B�#B�5B�TB�ZB�ZB�TB�TB�ZB�fB�B�B�B��B��B��B��B��B	  B	B	B	B	DB	�B	�B	!�B	%�B	+B	/B	5?B	49B	8RB	>wB	D�B	J�B	K�B	O�B	R�B	W
B	YB	[#B	]/B	^5B	^5B	`BB	`BB	aHB	dZB	gmB	l�B	m�B	o�B	r�B	t�B	w�B	z�B	{�B	|�B	}�B	}�B	}�B	�B	�B	�B	�B	�%B	�7B	�=B	�VB	�VB	�hB	�uB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�'B	�9B	�FB	�LB	�RB	�RB	�XB	�^B	�qB	�qB	�wB	�wB	��B	��B	��B	B	B	B	B	ÖB	ÖB	ÖB	ÖB	ÖB	ÖB	ĜB	ĜB	ĜB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�
B	�B	�#B	�)B	�/B	�/B	�5B	�5B	�5B	�;B	�BB	�BB	�NB	�ZB	�`B	�`B	�`B	�`B	�`B	�fB	�mB	�mB	�mB	�mB	�sB	�kB
g2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222224422222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 B��B��B��B��B��B�!B�?B�XB�}BBÖBÖBĜBŢBŢBƨBƨBǮBȴBȴBɺB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�/B�;B�B+B�B;dB\)Bn�B��BɺB��B�)B�HB�TB�ZB�fB�mB�B��BB��B�fB��B�B�fB�B�NB�qBǮB��B��BǮB�qB��B��B��B�B�RB�9B�B��B��B�\B�B}�B|�Bw�Bl�BcTB]/BP�BG�B>wB@�BB�B<jB'�B�BJB  B��B�B�5B��B��BȴBÖB�9B��B�DB�%B�B� Bt�BjBffBcTB\)BH�B=qB7LB'�BB
�yB
�/B
�
B
��B
��B
��B
�}B
�^B
�3B
��B
��B
�VB
�1B
�B
t�B
l�B
^5B
I�B
=qB
5?B
,B
oB	hsB��B��B��B��B��B��B	B	B	  B��B��B��B��B�B�B�`B�;B�B��B��BɺBĜB�qB�^B�FB�-B�B�B��B��B��B��B��B��B��B�{B�uB�oB�bB�\B�VB�JB�=B�1B�B�B~�Bz�Bu�Bq�Bo�Bk�BiyBffBffBdZBbNB_;B\)BW
BVBT�BR�BP�BN�BM�BQ�BS�BS�BS�BQ�BQ�BL�BL�BL�BJ�BI�BI�BH�BH�BF�BE�BC�BC�BC�BE�BG�BI�BI�BJ�BM�BO�BQ�BR�BR�BR�BQ�BO�BP�BO�BM�BO�BO�BN�BO�BQ�BP�BN�BM�BR�BW
BXBYBZB\)B_;BjBn�Bp�Bp�Bp�Bt�Bx�B~�B�B�B�1B�7B�=B�JB�oB��B��B��B��B��B�B�B�3B�^B�qB�}B��B��BĜBɺB��B��B��B��B��B�B�B�#B�5B�TB�ZB�ZB�TB�TB�ZB�fB�B�B�B��B��B��B��B��B	  B	B	B	B	DB	�B	�B	!�B	%�B	+B	/B	5?B	49B	8RB	>wB	D�B	J�B	K�B	O�B	R�B	W
B	YB	[#B	]/B	^5B	^5B	`BB	`BB	aHB	dZB	gmB	l�B	m�B	o�B	r�B	t�B	w�B	z�B	{�B	|�B	}�B	}�B	}�B	�B	�B	�B	�B	�%B	�7B	�=B	�VB	�VB	�hB	�uB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�'B	�9B	�FB	�LB	�RB	�RB	�XB	�^B	�qB	�qB	�wB	�wB	��B	��B	��B	B	B	B	B	ÖB	ÖB	ÖB	ÖB	ÖB	ÖB	ĜB	ĜB	ĜB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�
B	�B	�#B	�)B	�/B	�/B	�5B	�5B	�5B	�;B	�BB	�BB	�NB	�ZB	�`B	�`B	�`B	�`B	�`B	�fB	�mB	�mB	�mB	�mB	�sB	�kB
g2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222224422222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.04 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005191733                              AO  ARCAADJP                                                                    20181005191733    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005191733  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005191733  QCF$                G�O�G�O�G�O�C000            