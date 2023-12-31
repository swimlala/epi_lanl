CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-03-06T15:00:50Z creation      
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200306150050  20220204114424  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  7662                            2C  D   APEX                            8312                            080318                          846 @��*�W1   @�>���@7CS����b�bM��1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @�  @�  A   A   A>ffA`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DI��DJ� DK  DK� DL  DL� DM  DM� DN  DN� DOfDO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�fDy�)D�&fD�b�D��qD��\D�D�T{D��=D�ҏD�\D�O
D��RD��RD��D�V�Dڌ{D���D�=D�NfD�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @~�R@�\)@�\)A�A6zAW�Aw�A��
A��
A��
A��
A��
A��
A��
A��
B�B�B�B�B%�B-�B5�B=�BE�BM�BU�B]�Be�Bm�Bu�B}�B���B���B���B���B���B�B�B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���Cz�Cz�Cz�Cz�C	z�Cz�Cz�Cz�Cz�Cz�Cz�Cz�Cz�Cz�Cz�Cz�C!z�C#z�C%z�C'z�C)z�C+z�C-z�C/z�C1z�C3z�C5z�C7z�C9z�C;z�C=z�C?z�CAz�CCz�CEz�CGz�CIz�CKz�CMz�COz�CQz�CSz�CUz�CWz�CYz�C[z�C]z�C_z�Caz�Ccz�Cez�Cgz�Ciz�Ckz�Cmz�Coz�Cqz�Csz�Cuz�Cwz�Cyz�C{z�C}z�Cz�C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��>C��qC��qC��qC��qC��qC��qC��qC��qC��>C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC½qCýqCĽqCŽqCƽqCǽqCȽqCɽqCʽqC˽qC̽qCͽqCνqCϽqCнqCѽqCҽqCӽqCԽqCսqCֽqC׽qCؽqCٽqCڽqC۽qCܽqCݰ�C޽qC߽qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC��>C��qC�qC�qC�qC�qC�qC�qC��qC��qC��qC��qC���C���C��qC��qC��qC��qC��qC��qD ^�D ޸D^�D޸D^�D޸D^�D޸D^�D�RD^�D޸D^�D޸D^�D޸D^�D޸D	^�D	޸D
^�D
޸D^�D޸D^�D޸D^�D޸D^�D޸D^�D޸D^�D޸D^�D޸D^�D޸D^�D�DeD޸D^�D޸D^�D޸D^�D޸D^�D޸D^�D޸D^�D޸D^�D޸D^�D޸D^�D޸D^�D޸D^�D޸D ^�D ޸D!^�D!޸D"^�D"޸D#^�D#޸D$^�D$޸D%^�D%޸D&^�D&޸D'^�D'޸D(^�D(޸D)^�D)޸D*^�D*޸D+^�D+޸D,^�D,޸D-^�D-޸D.^�D.޸D/^�D/޸D0^�D0޸D1^�D1޸D2^�D2޸D3^�D3޸D4^�D4޸D5^�D5޸D6^�D6޸D7^�D7޸D8^�D8޸D9^�D9޸D:^�D:޸D;^�D;޸D<^�D<޸D=^�D=޸D>^�D>޸D?^�D?޸D@^�D@޸DA^�DA޸DB^�DB޸DC^�DC޸DD^�DD޸DE^�DE޸DF^�DF޸DG^�DG޸DH^�DH޸DI^�DI�RDJ^�DJ޸DK^�DK޸DL^�DL޸DM^�DM޸DN^�DN�DO^�DO޸DP^�DP޸DQ^�DQ޸DR^�DR޸DS^�DS޸DT^�DT޸DU^�DU޸DV^�DV޸DW^�DW޸DX^�DX޸DY^�DY޸DZ^�DZ޸D[^�D[޸D\^�D\޸D]^�D]޸D^^�D^޸D_^�D_޸D`^�D`޸Da^�Da޸Db^�Db޸Dc^�Dc޸Dd^�Dd޸De^�De޸Df^�Df޸Dg^�Dg޸Dh^�Dh޸Di^�Di޸Dj^�Dj޸Dk^�Dk޸Dl^�Dl޸Dm^�Dm޸Dn^�Dn޸Do^�Do޸Dp^�Dp޸Dq^�Dq޸Dr^�Dr޸Ds^�Ds޸Dt^�Dt�Dy��D��D�R=D���D�θD�pD�C�D���D���D��D�>fD�w�D�ϮD�
=D�FD�{�D��RD�	�D�=�D�vD��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�`BA�ZA�\)A�ZA�^5A�jA�l�A�l�A�jA�jA�l�A�ffA�9XA��mA�VA���A���A�~�A�C�A��yA�ƨA��A�$�A�M�A��A��A���A�  A�$�A�VA��mA���A�K�A�1A�`BA��RA�7LA�O�A�A�A�E�A��HA��TA��A�ƨA���A��A�ffA��^A��
A�^5A�XA��uA�~�A��DA���A�I�A���A�ȴA���A�oA�$�A��;A���A��7A�z�A�Q�A��
A�ZA�?}A���A�E�A��HA���A��;A�~�A�ƨA���A�A�A�r�A��FA��;A���A���A���A��A�;dA�`BA���A�ȴA�x�A~�RA|��A{�hAz��Av�\Arv�Ap��Am&�Ak��Ai�Ag�^AfAd��Acx�Aa�Aa%A`�yA`z�A^ĜA\�\AY\)AW��AVbNAUt�AS�AQoAP9XAO\)AN��AM�AL�!AKG�AIXAH~�AGdZAF�RAD�AC��AAƨA?�mA?A>bA;x�A:��A9��A8��A7��A7�A5+A2�A1��A1K�A0�uA/S�A-A+�PA*~�A)XA)+A'��A&�+A%�hA$z�A#p�A"1'A!7LA ��A =qAƨAoA��A��A+A��A��A��A�A�A;dA�RA��A%A��A1Ar�A�;A��A�mAC�AZA�;A�A~�A9XA�hA
��A
�RA
�+A
�A	t�A�AjA�Ar�A  A��A�AĜA��A=qA��A�A\)A �jA �A E�A  �@�ƨ@���@�hs@���@���@���@���@�-@�x�@�&�@�9X@�!@���@���@���@�w@���@���@�  @��@�@�h@��@��y@��`@�bN@ݩ�@�Q�@��m@�o@�&�@�9X@�=q@�V@���@�\)@�~�@��@��@��y@�ff@���@�`B@̃@�V@�Ĝ@�o@�p�@ŉ7@�x�@��
@��@��/@�j@�Ĝ@�K�@�V@��^@�X@�  @��R@�@�X@���@�9X@���@�dZ@��+@���@�z�@��m@���@�K�@���@��T@�V@�ƨ@�ff@�Q�@�|�@��!@�5?@��h@�7L@�G�@��7@��j@�j@��@�K�@��@�|�@�;d@��@���@���@�ȴ@�"�@���@��@��@��@��u@�r�@�1'@���@���@���@�t�@��@���@�C�@���@�5?@�?}@��@��D@���@�7L@��`@��@���@��u@��@�/@�&�@�j@��j@�E�@���@�|�@� �@���@�V@��@��@��@�z�@�bN@��D@�S�@�33@�~�@��T@�ff@���@�@�33@�"�@�C�@��@�  @�
=@���@���@��u@��@�ȴ@�M�@�v�@�n�@�$�@��T@��#@�J@�5?@�-@���@�Ĝ@�9X@�9X@�1'@�l�@�"�@�ȴ@��+@�^5@�$�@��T@�X@���@�?}@��`@��D@���@��u@���@��D@�1@���@�t�@��y@��R@��R@�E�@�M�@�E�@���@�`B@��@��/@�bN@�1'@�1'@��;@�\)@�o@��y@�=q@�v�@�
=@��!@�v�@�M�@���@��7@���@���@���@�z�@�Z@�(�@��@��@��w@��P@�S�@�;d@�o@��y@��R@���@�J@��^@��h@�X@��@���@���@�z�@�A�@��
@�S�@�
=@��@��R@���@���@��+@�V@�V@�V@��@��@���@��-@�p�@�?}@��`@�z�@��@��
@��w@���@���@��P@��P@�|�@�t�@�C�@��y@���@��R@�v�@�V@�=q@���@��7@�O�@�&�@��`@���@~�@y��@q@h�@a=�@Z͟@T�.@K��@EF@?C�@8�@38@/�@*�R@&�6@!IR@Xy@�$@�@;d@	�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A�`BA�ZA�\)A�ZA�^5A�jA�l�A�l�A�jA�jA�l�A�ffA�9XA��mA�VA���A���A�~�A�C�A��yA�ƨA��A�$�A�M�A��A��A���A�  A�$�A�VA��mA���A�K�A�1A�`BA��RA�7LA�O�A�A�A�E�A��HA��TA��A�ƨA���A��A�ffA��^A��
A�^5A�XA��uA�~�A��DA���A�I�A���A�ȴA���A�oA�$�A��;A���A��7A�z�A�Q�A��
A�ZA�?}A���A�E�A��HA���A��;A�~�A�ƨA���A�A�A�r�A��FA��;A���A���A���A��A�;dA�`BA���A�ȴA�x�A~�RA|��A{�hAz��Av�\Arv�Ap��Am&�Ak��Ai�Ag�^AfAd��Acx�Aa�Aa%A`�yA`z�A^ĜA\�\AY\)AW��AVbNAUt�AS�AQoAP9XAO\)AN��AM�AL�!AKG�AIXAH~�AGdZAF�RAD�AC��AAƨA?�mA?A>bA;x�A:��A9��A8��A7��A7�A5+A2�A1��A1K�A0�uA/S�A-A+�PA*~�A)XA)+A'��A&�+A%�hA$z�A#p�A"1'A!7LA ��A =qAƨAoA��A��A+A��A��A��A�A�A;dA�RA��A%A��A1Ar�A�;A��A�mAC�AZA�;A�A~�A9XA�hA
��A
�RA
�+A
�A	t�A�AjA�Ar�A  A��A�AĜA��A=qA��A�A\)A �jA �A E�A  �@�ƨ@���@�hs@���@���@���@���@�-@�x�@�&�@�9X@�!@���@���@���@�w@���@���@�  @��@�@�h@��@��y@��`@�bN@ݩ�@�Q�@��m@�o@�&�@�9X@�=q@�V@���@�\)@�~�@��@��@��y@�ff@���@�`B@̃@�V@�Ĝ@�o@�p�@ŉ7@�x�@��
@��@��/@�j@�Ĝ@�K�@�V@��^@�X@�  @��R@�@�X@���@�9X@���@�dZ@��+@���@�z�@��m@���@�K�@���@��T@�V@�ƨ@�ff@�Q�@�|�@��!@�5?@��h@�7L@�G�@��7@��j@�j@��@�K�@��@�|�@�;d@��@���@���@�ȴ@�"�@���@��@��@��@��u@�r�@�1'@���@���@���@�t�@��@���@�C�@���@�5?@�?}@��@��D@���@�7L@��`@��@���@��u@��@�/@�&�@�j@��j@�E�@���@�|�@� �@���@�V@��@��@��@�z�@�bN@��D@�S�@�33@�~�@��T@�ff@���@�@�33@�"�@�C�@��@�  @�
=@���@���@��u@��@�ȴ@�M�@�v�@�n�@�$�@��T@��#@�J@�5?@�-@���@�Ĝ@�9X@�9X@�1'@�l�@�"�@�ȴ@��+@�^5@�$�@��T@�X@���@�?}@��`@��D@���@��u@���@��D@�1@���@�t�@��y@��R@��R@�E�@�M�@�E�@���@�`B@��@��/@�bN@�1'@�1'@��;@�\)@�o@��y@�=q@�v�@�
=@��!@�v�@�M�@���@��7@���@���@���@�z�@�Z@�(�@��@��@��w@��P@�S�@�;d@�o@��y@��R@���@�J@��^@��h@�X@��@���@���@�z�@�A�@��
@�S�@�
=@��@��R@���@���@��+@�V@�V@�V@��@��@���@��-@�p�@�?}@��`@�z�@��@��
@��w@���@���@��P@��P@�|�@�t�@�C�@��y@���@��R@�v�@�V@�=q@���@��7@�O�@�&�@��`G�O�@~�@y��@q@h�@a=�@Z͟@T�.@K��@EF@?C�@8�@38@/�@*�R@&�6@!IR@Xy@�$@�@;d@	�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB\)B\)B\)B]/B\)B\)B\)B]/B]/B]/B]/B_;BgmBu�B�JB��B�jB��B�B�NB�NB�TB�B�B9XB=qBO�Bm�B�%B�hB��B��B��B�B��B��B��B�%B��B��B��B��B��B��B�\B{�BiyB`BBW
B@�B8RB33B1'B0!B(�B#�B�B{BB��B�sB�ZB�NB�BB�;B�/B�BǮB�!B�B��B��B�uB�7Bt�BYB=qB.B �BuB%B
��B
�NB
��B
�FB
�!B
��B
�DB
s�B
aHB
K�B
@�B
6FB
-B
hB	�B	�B	B	�9B	��B	��B	�=B	�B	v�B	n�B	gmB	ffB	dZB	ZB	J�B	;dB	2-B	,B	%�B	�B	hB	PB		7B	B	B��B��B�B�B�mB�NB�)B�B��BɺBB�}B�'B��B�B��B��B��B��B�7B�B|�B|�Bz�Bq�BhsBdZB\)BXBS�BT�BXBQ�BVBO�BG�BA�BA�BB�BE�BD�BC�BC�BB�BC�BE�BJ�BH�BB�BA�B@�B=qB=qB9XB9XB6FB8RB49B49B33B1'B1'B0!B/B/B-B-B,B,B+B)�B(�B'�B&�B$�B$�B$�B#�B"�B"�B!�B!�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B�B�B�B �B"�B%�B&�B&�B'�B&�B&�B'�B&�B)�B,B-B2-B2-B2-B33B5?B49B6FB8RB9XB9XB9XB8RB9XB8RB8RB8RB8RB7LB;dB<jB@�BC�BF�BO�BW
BZB^5B^5BbNBiyBiyBm�Bo�Bp�Bq�Bq�Bu�B{�B}�B}�B|�B}�B� B�B�B�B�%B�1B�=B�JB�VB�VB�oB�{B�hB�oB�uB�{B��B��B��B��B��B��B�B�9B�RB�jB��BŢB��B��B�B�B�BB�ZB�mB�sB�B�B�B�B��B	B	B	B	B	%B		7B	
=B	DB	PB	�B	�B	�B	�B	�B	�B	"�B	%�B	%�B	(�B	8RB	=qB	D�B	L�B	T�B	YB	YB	YB	W
B	I�B	I�B	M�B	K�B	K�B	K�B	J�B	O�B	R�B	XB	]/B	^5B	`BB	dZB	ffB	jB	aHB	bNB	e`B	ffB	cTB	cTB	e`B	gmB	gmB	gmB	hsB	jB	m�B	o�B	q�B	n�B	n�B	p�B	r�B	u�B	{�B	|�B	{�B	}�B	� B	� B	�B	�B	�1B	�1B	�7B	�VB	�VB	�VB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�'B	�'B	�-B	�3B	�?B	�jB	�qB	�qB	�wB	�}B	��B	ÖB	ƨB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�#B	�#B	�)B	�5B	�5B	�BB	�BB	�HB	�NB	�TB	�ZB	�`B	�fB	�fB	�fB	�fB	�mB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
�B
VB
�B
QB
"4B
,"B
4B
<B
A�B
J�B
O�B
U�B
Y�B
]�B
`vB
f�B
i�B
m�B
m]B
poB
v�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  BN�BN�BN�BO�BN�BN�BN�BO�BO�BO�BO�BQ�BY�BhDB~�B�lB��B�LBʈB��B��B��B�B/B+�B/�BBLB_�Bx�B��B��B�#B�`B�~B�`B�B��Bx�B��B�BB�<B�B�B��B��BnRB[�BR�BIzB2�B*�B%�B#�B"�BmBOB1B�B��B�GB��B��B��B��BѼBϱBȆB�3B��B��B�NB�$B� B{�BgKBK�B0B �B^BB
��B
�[B
��B
�qB
��B
��B
�nB
}�B
feB
S�B
>{B
39B
(�B
�B
$B	�@B	��B	�UB	�B	��B	�^B	}
B	t�B	i�B	aiB	Z?B	Y8B	W,B	L�B	=�B	.<B	%B	�B	�B	�B	FB	 /B�B��B��B��B�B�B�cB�RB�3B�B��B��B��B�yB�hB�B��B��B��B��B��B�rB|*Bu Bo�Bo�Bm�Bd�B[kBWSBO#BK
BF�BG�BKBD�BI BB�B:�B4�B4�B5�B8�B7�B6�B6�B5�B6�B8�B=�B;�B5�B4�B3�B0sB0sB,[B,[B)JB+VB'>B'>B&8B$,B$,B#'B"!B"!B B BBB	BB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B
BB B%;B%;B%;B&AB(MB'GB)TB+`B,fB,fB,fB+aB,gB+aB+aB+aB+aB*\B.sB/zB3�B6�B9�BB�BJBM+BQBBQBBU[B\�B\�B`�Bb�Bc�Bd�Bd�Bh�Bn�Bp�Bp�Bo�Bq BsBvBx*Bx*By0B{<B}HBUB�aB�aB�zB��B�sB�zB��B��B��B��B��B��B��B� B�B�AB�ZB�rB��B��B��B��B�	B�B�FB�^B�qB�wBވB�B�B�B��B�B� B� B� B�&B�8B�>B�EB	 PB	�B	
�B	�B	�B	�B	�B	�B	�B	�B	�B	+NB	0lB	7�B	?�B	G�B	LB	LB	LB	JB	<�B	<�B	@�B	>�B	>�B	>�B	=�B	B�B	E�B	K	B	P'B	Q-B	S:B	WRB	Y]B	]vB	T@B	UFB	XXB	Y^B	VLB	VLB	XXB	ZeB	ZeB	ZeB	[kB	]wB	`�B	b�B	d�B	a�B	a�B	c�B	e�B	h�B	n�B	o�B	n�B	p�B	r�B	r�B	s�B	vB	{&B	{&B	|,B	�KB	�KB	�KB	�iB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�%B	�1B	�[B	�bB	�bB	�hB	�nB	�tB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�#B	�#B	�0B	�0B	�6B	�<B	�BB	�HB	�NB	�TB	�TB	�TB	�TB	�[B	�`B	�fB	�fB	�lB	�lB	�rB	�rB	�xB	�~B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��G�O�B	��B
AB
�B
;B
B
B
'B
.�B
4qB
=�B
B�B
HhB
L�B
P�B
S\B
YgB
\�B
`vB
`BB
cTB
i�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.52 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9997(+/-0.0001), vertically averaged dS =-0.013(+/-0.005) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144242022020411442420220204114424  AO  ARCAADJP                                                                    20200306150050    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200306150050  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200306150050  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114424  IP                  G�O�G�O�G�O�                