CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:16:53Z creation      
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
_FillValue                 �  A8   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C,   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  T�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \x   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ^l   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  f4   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h(   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  o�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  w�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  y�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �t   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �h   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �0   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �`   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �`   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �`   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �`   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �    HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181005191653  20181005191653  5904954 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               A   AO  6557                            2B  A   APEX                            7468                            062512                          846 @׮��y1   @׮�W:۬@4=�E���c��t�j1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      A   A   A   @9��@�  @�  A   A   A>ffA`  A�  A�  A�  A�  A�  A�  A�  A���B   B  B  B  B   B(  B/��B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�33B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C�C�C  C�fC  C  C   C!�fC#�fC&  C(  C*  C,  C.  C/�fC1�fC4  C6�C8  C9�fC<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ�C\  C^  C`  Cb  Cc�fCe�fCh  Cj  Cl  Cn  Cp�Cr�Ct  Cv  Cx  Cz  C|  C~�C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C��3C�  C��C��C��C�  C�  C�  C��3C�  C��C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C��C�  C�  C��3C�  C�  C�  C��C�  C��C��C��C�  C�  C�  C�  C�  C�  C��3C��C��C�  C��3C��3C�  C�  C�  C�  C��3C��3C��3C�  C��C��C�  C��3C�  C��C��C�  C�  C�  C��C��C�  C�  C��C��3C�  C��C�  C��3C��fC�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C��C��3C��fC��3C�  C��C��3C�  C�  C�  C��C�  D fD y�DfDy�D  Dy�D  D��DfD� DfD�fD��D� D��D�fD  D� D	  D	y�D
fD
y�D  Dy�D  D� D  Dy�D��D�fD��D� D��D� DfD�fD��Ds3D��Dy�D  D��D  D� D  Dy�D  D�fD  D� D��D�fD  Dy�DfD� D  D�fDfDs3D  D� D  D�fD��D � D!  D!y�D"  D"y�D#  D#y�D$  D$� D%  D%y�D&  D&y�D'  D'�fD(  D(� D)  D)y�D)��D*� D+  D+� D,  D,� D-  D-y�D-��D.�fD/fD/y�D0  D0�fD1  D1�fD2fD2�fD3fD3�fD3��D4�fD5fD5� D5��D6� D7  D7� D8  D8� D8��D9y�D9��D:y�D;  D;�fD;��D<y�D<��D=� D>  D>� D>��D?y�D?��D@� DA  DAy�DB  DB�fDB��DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DI�3DJy�DK  DKy�DLfDL� DM  DM� DM��DN�fDOfDO� DPfDP� DQ  DQ� DR  DRy�DSfDS� DS��DT�fDU  DU� DV  DVy�DW  DW� DX  DX� DY  DY� DZfDZ�fD[fD[� D[��D\y�D]  D]�fD^  D^� D_fD_� D`  D`� DafDa�fDa��Dby�Db��Dc� Dc��Ddy�De  De� Df  Df� Df��Dgy�Dg�3Dh� DifDi�fDi��Djy�Dk  Dk� DlfDl�fDm  Dmy�DnfDns3Dn��Doy�Dp  Dp� Dp��Dq��Dr  Dr�fDr��Dsy�Ds��Dty�Dt��Du� DvfDvy�Dw  Dwy�DwٚDy� D�>�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @=p�@��@��A ��A ��A?\)A`��A�z�A�z�A�z�A�z�A�z�A�z�A�z�A�G�B =qB=qB=qB=qB =qB(=qB/�B8=qB@=qBH=qBP=qBX=qB`=qBh=qBp=qBx=qB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�Q�B��B��B��B��C \C\C\C\C\C
\C\C\C\C\C(�C(�C\C��C\C\C \C!��C#��C&\C(\C*\C,\C.\C/��C1��C4\C6(�C8\C9��C<\C>\C@\CB\CD\CF\CH\CJ\CL\CN\CP\CR\CT\CV\CX\CZ(�C\\C^\C`\Cb\Cc��Ce��Ch\Cj\Cl\Cn\Cp(�Cr(�Ct\Cv\Cx\Cz\C|\C~(�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C��C��C��C���C��C�{C�{C�{C��C��C��C���C��C�{C��C��C��C���C��C��C��C��C��C��C�{C�{C��C��C��C��C�{C��C��C���C��C��C��C�{C��C�{C�{C�{C��C��C��C��C��C��C���C�{C�{C��C���C���C��C��C��C��C���C���C���C��C�{C�{C��C���C��C�{C�{C��C��C��C�{C�{C��C��C�{C���C��C�{C��C���C��C��C��C��C���C��C��C��C��C��C��C��C��C�{C��C��C��C�{C���C��C���C��C�!HC���C��C��C��C�{C��D 
=D }qD
=D}qD�D}qD�D��D
=D��D
=D�=D�qD��D�qD�=D�D��D	�D	}qD

=D
}qD�D}qD�D��D�D}qD�qD�=D�qD��D�qD��D
=D�=D�qDw
D�qD}qD�D��D�D��D�D}qD�D�=D�D��D�qD�=D�D}qD
=D��D�D�=D
=Dw
D�D��D�D�=D�qD ��D!�D!}qD"�D"}qD#�D#}qD$�D$��D%�D%}qD&�D&}qD'�D'�=D(�D(��D)�D)}qD)�qD*��D+�D+��D,�D,��D-�D-}qD-�qD.�=D/
=D/}qD0�D0�=D1�D1�=D2
=D2�=D3
=D3�=D3�qD4�=D5
=D5��D5�qD6��D7�D7��D8�D8��D8�qD9}qD9�qD:}qD;�D;�=D;�qD<}qD<�qD=��D>�D>��D>�qD?}qD?�qD@��DA�DA}qDB�DB�=DB�qDC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DI�
DJ}qDK�DK}qDL
=DL��DM�DM��DM�qDN�=DO
=DO��DP
=DP��DQ�DQ��DR�DR}qDS
=DS��DS�qDT�=DU�DU��DV�DV}qDW�DW��DX�DX��DY�DY��DZ
=DZ�=D[
=D[��D[�qD\}qD]�D]�=D^�D^��D_
=D_��D`�D`��Da
=Da�=Da�qDb}qDb�qDc��Dc�qDd}qDe�De��Df�Df��Df�qDg}qDg�
Dh��Di
=Di�=Di�qDj}qDk�Dk��Dl
=Dl�=Dm�Dm}qDn
=Dnw
Dn�qDo}qDp�Dp��Dp�qDq��Dr�Dr�=Dr�qDs}qDs�qDt}qDt�qDu��Dv
=Dv}qDw�Dw}qDw�qDy��D�@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Aӟ�Aӡ�Aӡ�Aӡ�Aӡ�Aӡ�Aӣ�Aӣ�Aӣ�Aӥ�Aӥ�Aӧ�Aө�AӬAӣ�Aӥ�AӇ+A��A��#A���A҅A��mA�7LA�ȴA�v�A�bAϣ�A�l�A� �A��A��A�oA�oA�VA�%A��mA��/AΗ�A�hsA�VA�=qA�$�A��A���A��#A͉7A���A�ZA��A�G�A�AǮA�{A��`AƗ�AŸRA�+Aĩ�A�M�A��A�^5A¸RA�/A�r�A��A��\A��A���A�+A�~�A��A��mA�Q�A���A�$�A��wA���A�\)A���A�=qA�ĜA��A�?}A�I�A�A�JA��yA��A���A��A���A�ZA���A��PA��^A�=qA�A�VA��RA�A���A�|�A�{A���A��A�{A���A��`A�%A���A��DA�ƨA�A��jA~Q�A|A�Az�Ay��Aw��AvM�Au�Ar��Aql�An^5Ai/Ae�mAc�;Aap�A`A�A^�AY��AW�AS33AP-AOK�AN�AM�AL�9AJ��AI�AHbNAG�-AGG�AF�yAE\)AC�AAhsA?K�A=\)A;O�A:ĜA9�
A9`BA8bNA4��A2�DA1
=A/+A-�
A-�hA-A,A�A,JA+�A)�A'��A%�;A$��A#��A"I�A"A!�#A �uA�uA�#A-A9XA  A�#A��A��A��A�A(�A��A��A�A{A�TA/A1'A��A�-AȴA(�A��A�A`BAE�Ap�A7LA�Ar�A&�A	��A�uA|�A7LA�-A��AXA�A�-A�
AbA�`AVA��A�#A^5Ax�A��A;dA1A ��@�Q�@�  @�{@�(�@���@���@�@�&�@�%@��/@�D@땁@蛦@� �@��@��@�A�@�"�@�b@��@��/@�I�@�Q�@��`@�V@�%@��@��/@�Ĝ@���@�j@�n�@�C�@���@�{@с@�j@�9X@�b@υ@��@��H@���@�ȴ@���@�n�@�p�@���@��/@���@�Ĝ@�Ĝ@̴9@̬@̣�@�r�@�ƨ@���@���@�o@Ə\@�V@���@ř�@�bN@�E�@�$�@�J@��T@�x�@�z�@�o@�@�-@��@�bN@�j@�(�@��@�^5@�`B@��`@��@���@���@��@��@��@�Ĝ@��D@�9X@��@��@��@�@��R@�M�@���@��@�hs@�G�@�V@��/@�Q�@��
@�t�@�K�@�C�@�"�@���@��T@�@���@���@���@���@�O�@�X@� �@�ȴ@��h@��@���@�K�@�+@��@���@�=q@��-@���@��9@�r�@�I�@�(�@�b@��@��H@�-@���@��@���@�bN@��w@�t�@���@���@�~�@�=q@��@��^@��7@�`B@�&�@���@��@�r�@��@��@���@��@�\)@�+@��y@���@���@��+@�~�@�M�@�$�@�@��@��@��T@��h@�?}@��@��`@��j@��u@�9X@��@���@���@�|�@�l�@�\)@�"�@��R@�5?@���@��@���@��h@�p�@�`B@�X@�G�@�&�@�%@���@��@��F@�\)@�;d@�"�@��@�@��@���@�5?@���@��@��@��-@�x�@�?}@��/@�Q�@�  @��;@��;@��
@��w@���@�S�@�"�@�@��\@�@��^@��-@��-@���@�p�@�X@�O�@���@��j@�z�@��w@�l�@�K�@�C�@�C�@�;d@�+@�o@�@��@���@���@���@�v�@�^5@�$�@��#@���@�X@�?}@�?}@��@���@��@�A�@�b@��
@��@�33@��H@��!@�^5@�\�@w�4111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  Aӟ�Aӡ�Aӡ�Aӡ�Aӡ�Aӡ�Aӣ�Aӣ�Aӣ�Aӥ�Aӥ�Aӧ�Aө�AӬAӣ�Aӥ�AӇ+A��A��#A���A҅A��mA�7LA�ȴA�v�A�bAϣ�A�l�A� �A��A��A�oA�oA�VA�%A��mA��/AΗ�A�hsA�VA�=qA�$�A��A���A��#A͉7A���A�ZA��A�G�A�AǮA�{A��`AƗ�AŸRA�+Aĩ�A�M�A��A�^5A¸RA�/A�r�A��A��\A��A���A�+A�~�A��A��mA�Q�A���A�$�A��wA���A�\)A���A�=qA�ĜA��A�?}A�I�A�A�JA��yA��A���A��A���A�ZA���A��PA��^A�=qA�A�VA��RA�A���A�|�A�{A���A��A�{A���A��`A�%A���A��DA�ƨA�A��jA~Q�A|A�Az�Ay��Aw��AvM�Au�Ar��Aql�An^5Ai/Ae�mAc�;Aap�A`A�A^�AY��AW�AS33AP-AOK�AN�AM�AL�9AJ��AI�AHbNAG�-AGG�AF�yAE\)AC�AAhsA?K�A=\)A;O�A:ĜA9�
A9`BA8bNA4��A2�DA1
=A/+A-�
A-�hA-A,A�A,JA+�A)�A'��A%�;A$��A#��A"I�A"A!�#A �uA�uA�#A-A9XA  A�#A��A��A��A�A(�A��A��A�A{A�TA/A1'A��A�-AȴA(�A��A�A`BAE�Ap�A7LA�Ar�A&�A	��A�uA|�A7LA�-A��AXA�A�-A�
AbA�`AVA��A�#A^5Ax�A��A;dA1A ��@�Q�@�  @�{@�(�@���@���@�@�&�@�%@��/@�D@땁@蛦@� �@��@��@�A�@�"�@�b@��@��/@�I�@�Q�@��`@�V@�%@��@��/@�Ĝ@���@�j@�n�@�C�@���@�{@с@�j@�9X@�b@υ@��@��H@���@�ȴ@���@�n�@�p�@���@��/@���@�Ĝ@�Ĝ@̴9@̬@̣�@�r�@�ƨ@���@���@�o@Ə\@�V@���@ř�@�bN@�E�@�$�@�J@��T@�x�@�z�@�o@�@�-@��@�bN@�j@�(�@��@�^5@�`B@��`@��@���@���@��@��@��@�Ĝ@��D@�9X@��@��@��@�@��R@�M�@���@��@�hs@�G�@�V@��/@�Q�@��
@�t�@�K�@�C�@�"�@���@��T@�@���@���@���@���@�O�@�X@� �@�ȴ@��h@��@���@�K�@�+@��@���@�=q@��-@���@��9@�r�@�I�@�(�@�b@��@��H@�-@���@��@���@�bN@��w@�t�@���@���@�~�@�=q@��@��^@��7@�`B@�&�@���@��@�r�@��@��@���@��@�\)@�+@��y@���@���@��+@�~�@�M�@�$�@�@��@��@��T@��h@�?}@��@��`@��j@��u@�9X@��@���@���@�|�@�l�@�\)@�"�@��R@�5?@���@��@���@��h@�p�@�`B@�X@�G�@�&�@�%@���@��@��F@�\)@�;d@�"�@��@�@��@���@�5?@���@��@��@��-@�x�@�?}@��/@�Q�@�  @��;@��;@��
@��w@���@�S�@�"�@�@��\@�@��^@��-@��-@���@�p�@�X@�O�@���@��j@�z�@��w@�l�@�K�@�C�@�C�@�;d@�+@�o@�@��@���@���@���@�v�@�^5@�$�@��#@���@�X@�?}@�?}@��@���@��@�A�@�b@��
@��@�33@��H@��!@�^5@�\�@w�4111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�}B
��B
��B
�
BB<jB`BBu�B�DB��B�uB�VB�PB�PB�VB�\B�bB�\B��B��B�!B�3B�9B�FB�RB�^B�^B�XB�^B�^B�?B��B��B��B��B�BB�BB#�B?}BO�B[#BffBv�B�+B�\B��B��B��B�B�?B�dB��B�`B�B�B��B��B��B��B��B��B�fBŢB��B��B��B�%B]/B(�B�BuB(�B/B-B)�B$�BuBVBB�)B�wB��B��B�VBk�BJ�B%�B{BbB
��B
�5B
ĜB
��B
�=B
v�B
YB
0!B
!�B
�B
PB	��B	��B	�B	�B	��B	�XB	��B	�B	x�B	l�B	cTB	W
B	=qB	/B	�B	\B	JB		7B	+B	B��B��B��B�B�B�B�sB�NB�)B�
B��B��B��B��BȴB��B��BŢB�jB�9B�B�?BŢB��B��B��B��B�B�;B�HB�NB�B��B��B��B�B�B��B�NB�yB�yB�fB�BB��BȴB��B��B�B��B��B��B�LB�B��B��B��B�B�B�B�!B��B��B�B��B��B��B�?B�!B�B�B��B��B��B��B�XB	B	B�B	  B	�B	/B	-B	)�B	(�B	�B	oB		7B	  B�ZB�/B�
B��B��B��B��B��B��B��B��B��BɺBŢB�B�B�B�?B�?B�-B�!B�LBÖBƨBǮB��B��B��B��B��B��BŢBBBBÖBÖBÖBĜBŢBƨBƨBǮB��B�B�5B�;B�BB�BB�NB�TB�TB�ZB�ZB�`B�mB�sB�B�B�B�B�B��B��B��B	B	B	B	B	+B	JB	DB	JB	oB	{B	�B	�B	�B	�B	#�B	$�B	(�B	)�B	,B	-B	5?B	8RB	9XB	:^B	<jB	<jB	=qB	A�B	E�B	H�B	J�B	L�B	P�B	Q�B	S�B	XB	[#B	`BB	aHB	cTB	e`B	hsB	l�B	p�B	r�B	t�B	z�B	� B	�B	�B	�1B	�VB	�DB	�7B	�%B	�%B	�+B	�+B	�1B	�7B	�=B	�JB	�VB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�-B	�9B	�9B	�?B	�?B	�FB	�^B	�dB	�qB	�qB	�qB	�wB	�}B	��B	ÖB	ŢB	ƨB	ǮB	ȴB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�B	�B	�/B	�5B	�5B	�5B	�;B	�;B	�;B	�BB	�BB	�BB	�HB	�HB	�HB	�mB	�sB	�yB	�yB	�yB	�B	�B	�B	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
1B
zB
q222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�}B
��B
��B
�
BB<jB`BBu�B�DB��B�uB�VB�PB�PB�VB�\B�bB�\B��B��B�!B�3B�9B�FB�RB�^B�^B�XB�^B�^B�?B��B��B��B��B�BB�BB#�B?}BO�B[#BffBv�B�+B�\B��B��B��B�B�?B�dB��B�`B�B�B��B��B��B��B��B��B�fBŢB��B��B��B�%B]/B(�B�BuB(�B/B-B)�B$�BuBVBB�)B�wB��B��B�VBk�BJ�B%�B{BbB
��B
�5B
ĜB
��B
�=B
v�B
YB
0!B
!�B
�B
PB	��B	��B	�B	�B	��B	�XB	��B	�B	x�B	l�B	cTB	W
B	=qB	/B	�B	\B	JB		7B	+B	B��B��B��B�B�B�B�sB�NB�)B�
B��B��B��B��BȴB��B��BŢB�jB�9B�B�?BŢB��B��B��B��B�B�;B�HB�NB�B��B��B��B�B�B��B�NB�yB�yB�fB�BB��BȴB��B��B�B��B��B��B�LB�B��B��B��B�B�B�B�!B��B��B�B��B��B��B�?B�!B�B�B��B��B��B��B�XB	B	B�B	  B	�B	/B	-B	)�B	(�B	�B	oB		7B	  B�ZB�/B�
B��B��B��B��B��B��B��B��B��BɺBŢB�B�B�B�?B�?B�-B�!B�LBÖBƨBǮB��B��B��B��B��B��BŢBBBBÖBÖBÖBĜBŢBƨBƨBǮB��B�B�5B�;B�BB�BB�NB�TB�TB�ZB�ZB�`B�mB�sB�B�B�B�B�B��B��B��B	B	B	B	B	+B	JB	DB	JB	oB	{B	�B	�B	�B	�B	#�B	$�B	(�B	)�B	,B	-B	5?B	8RB	9XB	:^B	<jB	<jB	=qB	A�B	E�B	H�B	J�B	L�B	P�B	Q�B	S�B	XB	[#B	`BB	aHB	cTB	e`B	hsB	l�B	p�B	r�B	t�B	z�B	� B	�B	�B	�1B	�VB	�DB	�7B	�%B	�%B	�+B	�+B	�1B	�7B	�=B	�JB	�VB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�-B	�9B	�9B	�?B	�?B	�FB	�^B	�dB	�qB	�qB	�qB	�wB	�}B	��B	ÖB	ŢB	ƨB	ǮB	ȴB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�B	�B	�/B	�5B	�5B	�5B	�;B	�;B	�;B	�BB	�BB	�BB	�HB	�HB	�HB	�mB	�sB	�yB	�yB	�yB	�B	�B	�B	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
1B
zB
q222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.06 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005191653                              AO  ARCAADJP                                                                    20181005191653    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005191653  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005191653  QCF$                G�O�G�O�G�O�8000            