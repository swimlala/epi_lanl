CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-06-18T14:14:10Z creation      
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200618141410  20220204114423  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               yA   AO  7662                            2C  D   APEX                            8312                            080318                          846 @��]!/v�1   @��]���2@6>��"���cC��%1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    yA   B   B   @���@���A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�ffB���B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C�fC  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL�CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Cs�fCv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DI��DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZfDZ� D[  D[� D\  D\� D\��D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt��Dy�RD�(RD�U�D��fD�ǮD�#�D�^D��RD���D�&�D�VfD��3D��qD�RD�O\DڐRD���D��D�]qD��D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @q�@�(�@�\)A�A7�AW�Aw�A��
A��
A��
A��
A��
A��
A��
A��
B�B�B�B�B%�B-�B5�B=�BE�BM�BU�B]�Be�Bm�Bu�B}�B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B�B���B���B���B���B���B�\)B�B���B���B���B���B���B���B���B���Cz�Cz�Cz�Cz�C	z�Cz�Cz�Cz�CaGCz�Cz�Cz�Cz�Cz�Cz�Cz�C!z�C#z�C%z�C'z�C)z�C+z�C-z�C/z�C1z�C3z�C5z�C7z�C9z�C;z�C=z�C?z�CAz�CCz�CEz�CGz�CIz�CK�{CMz�COz�CQz�CSz�CUz�CWz�CYz�C[z�C]z�C_z�Caz�Ccz�Cez�Cgz�Ciz�Ckz�Cmz�Coz�Cqz�CsaGCuz�Cwz�Cyz�C{z�C}z�Cz�C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC���C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC���C��qC��qC��qC��qC��qC��qC��qC��qC���C��qC��qC��qC��qC��qC��>C��qC��qC��qC��qC��qC��qC��qC��qC½qCýqCĽqCŽqCƽqCǽqCȽqCɽqCʽqC˽qC̽qCͽqCνqCϽqCнqCѽqCҽqCӽqCԽqCսqCֽqC׽qCؽqCٽqCڽqC۽qCܽqCݽqC޽qC߽qC�qC�qC�qC��>C�qC�qC�qC�qC�qC�qC�qC�qC�qC��qC�qC�qC�qC�qC�qC�qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qD ^�D ޸D^�D޸D^�D޸D^�D޸D^�D޸D^�D޸D^�D޸D^�D޸D^�D޸D	^�D	޸D
^�D
޸D^�D޸D^�D޸D^�D޸D^�D޸D^�D޸D^�D޸D^�D޸D^�D޸D^�D޸D^�D޸D^�D޸D^�D޸D^�D޸D^�D޸D^�D޸D^�D޸D^�D޸D^�D޸D^�D޸D^�D޸D^�D޸D ^�D ޸D!^�D!޸D"^�D"޸D#^�D#޸D$^�D$޸D%^�D%޸D&^�D&޸D'^�D'޸D(^�D(޸D)^�D)޸D*^�D*޸D+^�D+޸D,^�D,޸D-^�D-޸D.^�D.޸D/^�D/޸D0^�D0޸D1^�D1޸D2^�D2޸D3^�D3޸D4^�D4޸D5^�D5޸D6^�D6޸D7^�D7޸D8^�D8޸D9^�D9޸D:^�D:޸D;^�D;޸D<^�D<޸D=^�D=޸D>^�D>޸D?^�D?޸D@^�D@޸DA^�DA޸DB^�DB޸DC^�DC޸DD^�DD޸DE^�DE޸DF^�DF޸DG^�DG޸DH^�DH޸DI^�DI�RDJ^�DJ޸DK^�DK޸DL^�DL޸DM^�DM޸DN^�DN޸DO^�DO޸DP^�DP޸DQ^�DQ޸DR^�DR޸DS^�DS޸DT^�DT޸DU^�DU޸DV^�DV޸DW^�DW޸DX^�DX޸DY^�DY�DZ^�DZ޸D[^�D[޸D\^�D\�RD]^�D]޸D^^�D^޸D_^�D_޸D`^�D`޸Da^�Da޸Db^�Db޸Dc^�Dc޸Dd^�Dd޸De^�De޸Df^�Df޸Dg^�Dg޸Dh^�Dh޸Di^�Di޸Dj^�Dj޸Dk^�Dk޸Dl^�Dl޸Dm^�Dm޸Dn^�Dn޸Do^�Do޸Dp^�Dp޸Dq^�Dq޸Dr^�Dr޸Ds^�Ds޸Dt^�Dt��Dyw
D��D�ED���D��
D��D�MpD���D��=D�D�E�D���D���D��D�>�D��D��HD�)D�L�D�=D��H111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�K�A�M�A�M�A�K�A�O�A�O�A�Q�A�O�A�O�A�O�A�Q�A�S�A�O�A�Q�A�S�A�VA�XA�XA�ZA�^5A�^5A�ffA�hsA�jA�l�A�l�A�l�A�l�A�n�A�jA�jA�l�A�l�A�n�A�l�A�l�A�p�A�p�A�p�A�p�A�n�A�p�A�r�A�bNA�\)A�K�A�VA̅A�v�A��A�{A�x�A�5?A��;A��!A�33A�ZA�ffA�S�A��DA�-A�z�A���A���A�bNA��A���A�O�A��-A�t�A��A��A���A��A��A���A���A�~�A��jA��A��!A��A�=qA�p�A�dZA��-A��TA��9A��jA�`BA�jA���A�JA�(�A�oA�I�A��A�p�A���A���A�r�A�VA�l�A��mA�{A��TA���A��HA��mA�M�A�?}A�O�A��-A��A��+Ax�A|�Axv�Av�AvQ�Aut�At�jAs�Apv�Ak��Af��AbĜA`�\A]�A[��A[AZM�AY&�AVz�AR�!AP�AM�AK�-AIS�AG�#AF�AEdZAD��ADr�AC�PAB�uA@��A>��A=�TA<1'A;��A;\)A;�A8ȴA7l�A7�A6��A5�^A4��A2�A1
=A0ȴA/C�A0�A/hsA.��A-A-A+�A*bA)/A)dZA)?}A(�A'�;A'/A&�A&�\A%�A#��A �A&�AbNAbA��A�A�A�\AXAG�A�DAhsA�\A�^A�A
�Av�A��A;dA�uA�AƨAl�AK�A��AA $�@�l�@��7@�b@�ȴ@��@�ȴ@�O�@�z�@��
@�dZ@�S�@�5?@�1@��
@�S�@��y@�x�@�bN@���@��@�-@�z�@��@��@柾@�@�@�@��@�I�@�J@�z�@㝲@�P@�I�@��@�E�@�ƨ@�l�@��@ف@�z�@ו�@ָR@��@�O�@Լj@�V@�`B@�bN@�dZ@ͺ^@Η�@�E�@�1'@�(�@��@��y@�S�@�/@�Ĝ@ǝ�@Ƨ�@�Ĝ@�1@��^@�V@�(�@��!@�$�@�$�@���@��/@���@�ȴ@�?}@�|�@�x�@���@�hs@��@�;d@�+@�o@�=q@�V@�V@��@��@��F@���@���@�dZ@��!@�M�@���@�V@��@�  @��
@��u@�1'@�z�@���@�1@��P@��w@�;d@�C�@��P@���@�1@�^5@�l�@��F@���@�dZ@�+@�ȴ@��^@��T@��T@��-@�O�@�%@�r�@���@��D@�ƨ@��\@�r�@�^5@��@�A�@��9@��H@�/@���@�r�@�{@�~�@�S�@�1'@�`B@���@��@���@��7@�X@���@�bN@�-@�I�@��P@�n�@��@�@�`B@�ff@�ȴ@��\@�ff@��+@�X@��/@���@���@���@��@�/@��7@�A�@���@�l�@�n�@�-@��+@���@�K�@�V@���@���@�K�@���@�1@�A�@���@� �@�33@�@��@�ff@��!@���@�ȴ@���@��@�K�@���@�ƨ@��;@�l�@���@��R@�33@���@�J@�&�@��@��`@��@��9@���@�33@�"�@�~�@�-@�5?@�$�@��@�J@��@��h@��@��@���@�X@�`B@�?}@�/@��@���@��9@�I�@�1'@��@���@�dZ@�C�@�S�@�33@��@��@�M�@���@��-@�hs@�%@��D@�9X@�b@��m@�ƨ@���@�dZ@���@��y@��R@�n�@�=q@���@�X@��@��/@�Ĝ@��j@���@�bN@�A�@�1'@�1'@���@���@���@��P@�C�@��@��@��!@��+@�5?@��@�E�@x>B@qa�@io @a��@[33@TZ@N �@G,�@@�[@:@4�@-�H@'�f@#�&@6z@ԕ@:�@��@��@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A�K�A�M�A�M�A�K�A�O�A�O�A�Q�A�O�A�O�A�O�A�Q�A�S�A�O�A�Q�A�S�A�VA�XA�XA�ZA�^5A�^5A�ffA�hsA�jA�l�A�l�A�l�A�l�A�n�A�jA�jA�l�A�l�A�n�A�l�A�l�A�p�A�p�A�p�A�p�A�n�A�p�A�r�A�bNA�\)A�K�A�VA̅A�v�A��A�{A�x�A�5?A��;A��!A�33A�ZA�ffA�S�A��DA�-A�z�A���A���A�bNA��A���A�O�A��-A�t�A��A��A���A��A��A���A���A�~�A��jA��A��!A��A�=qA�p�A�dZA��-A��TA��9A��jA�`BA�jA���A�JA�(�A�oA�I�A��A�p�A���A���A�r�A�VA�l�A��mA�{A��TA���A��HA��mA�M�A�?}A�O�A��-A��A��+Ax�A|�Axv�Av�AvQ�Aut�At�jAs�Apv�Ak��Af��AbĜA`�\A]�A[��A[AZM�AY&�AVz�AR�!AP�AM�AK�-AIS�AG�#AF�AEdZAD��ADr�AC�PAB�uA@��A>��A=�TA<1'A;��A;\)A;�A8ȴA7l�A7�A6��A5�^A4��A2�A1
=A0ȴA/C�A0�A/hsA.��A-A-A+�A*bA)/A)dZA)?}A(�A'�;A'/A&�A&�\A%�A#��A �A&�AbNAbA��A�A�A�\AXAG�A�DAhsA�\A�^A�A
�Av�A��A;dA�uA�AƨAl�AK�A��AA $�@�l�@��7@�b@�ȴ@��@�ȴ@�O�@�z�@��
@�dZ@�S�@�5?@�1@��
@�S�@��y@�x�@�bN@���@��@�-@�z�@��@��@柾@�@�@�@��@�I�@�J@�z�@㝲@�P@�I�@��@�E�@�ƨ@�l�@��@ف@�z�@ו�@ָR@��@�O�@Լj@�V@�`B@�bN@�dZ@ͺ^@Η�@�E�@�1'@�(�@��@��y@�S�@�/@�Ĝ@ǝ�@Ƨ�@�Ĝ@�1@��^@�V@�(�@��!@�$�@�$�@���@��/@���@�ȴ@�?}@�|�@�x�@���@�hs@��@�;d@�+@�o@�=q@�V@�V@��@��@��F@���@���@�dZ@��!@�M�@���@�V@��@�  @��
@��u@�1'@�z�@���@�1@��P@��w@�;d@�C�@��P@���@�1@�^5@�l�@��F@���@�dZ@�+@�ȴ@��^@��T@��T@��-@�O�@�%@�r�@���@��D@�ƨ@��\@�r�@�^5@��@�A�@��9@��H@�/@���@�r�@�{@�~�@�S�@�1'@�`B@���@��@���@��7@�X@���@�bN@�-@�I�@��P@�n�@��@�@�`B@�ff@�ȴ@��\@�ff@��+@�X@��/@���@���@���@��@�/@��7@�A�@���@�l�@�n�@�-@��+@���@�K�@�V@���@���@�K�@���@�1@�A�@���@� �@�33@�@��@�ff@��!@���@�ȴ@���@��@�K�@���@�ƨ@��;@�l�@���@��R@�33@���@�J@�&�@��@��`@��@��9@���@�33@�"�@�~�@�-@�5?@�$�@��@�J@��@��h@��@��@���@�X@�`B@�?}@�/@��@���@��9@�I�@�1'@��@���@�dZ@�C�@�S�@�33@��@��@�M�@���@��-@�hs@�%@��D@�9X@�b@��m@�ƨ@���@�dZ@���@��y@��R@�n�@�=q@���@�X@��@��/@�Ĝ@��j@���@�bN@�A�@�1'@�1'@���@���@���@��P@�C�@��@��@��!@��+@�5?G�O�@�E�@x>B@qa�@io @a��@[33@TZ@N �@G,�@@�[@:@4�@-�H@'�f@#�&@6z@ԕ@:�@��@��@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
��B
��B
��B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
��B
��B
��B
��B
B
B
B
B
B
B
B
��B
��B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
ÖB
ĜB
�B
�B
��B%B+BPBbB&�BN�B`BBv�B�B�B�1B��B��B��B�3B�XB��B�/B�fB�B��BVB{B�B&�B)�B#�B$�B2-B33B)�B0!B0!B0!B6FB.B�BPB�BhB+B  B�B�fBǮB��B}�BhsBk�BXB �B
��B
�B
ŢB
�'B
��B
�oB
�B
l�B
YB
H�B
33B
�B
uB
JB	��B	�`B	�
B	��B	��B	ɺB	��B	�!B	�oB	o�B	XB	D�B	9XB	+B	%�B	 �B	�B	\B��B�B�BB�
B��B��BƨB��B�wBĜBǮBŢB��B�?B�-B�-B�9B�dB��BǮB�^BƨB�B��B�
BŢB�FB��B�jB��B�B�B�B�
B��BȴBĜB��B�5B�TB�mB�`B�BB�)B��BB��B�{Bo�BYBVBVB[#B^5B]/BP�BT�B`BB[#B`BBVB;dB1'B.B,B+B)�B)�B,B-B+B,B)�B'�B)�B)�B+B,B+B-B-B.B/B0!B5?B-B-B,B,B/B/B.B.B/B0!B0!B33B5?B7LB=qBI�BcTBe`BaHBcTBaHBcTBk�Bp�Bn�Bm�B`BBbNB`BB^5BcTBdZBcTBbNBaHB]/BXB[#BaHB\)B_;BgmBhsBe`BdZBe`Bp�B�B�%B�JB�PB�JB�JB�\B�VB�JB�DB�DB�JB�VB�JB�\B�oB�\B�\B�\B�JB�7B�7B�DB�\B�hB��B��B��B��B��B��B��B��B��B�B�B�!B�'B�-B�?B�FB�jB�}BŢB��B��B��B�
B�B�5B�B�B�5B�B��B��B��B��B��B	B	  B	B	B	B	B	1B	+B	%B	
=B	DB	1B	B��B	B	\B	oB	bB	1B	+B	+B	hB	�B	�B	"�B	.B	2-B	33B	49B	6FB	7LB	8RB	:^B	6FB	0!B	1'B	1'B	2-B	8RB	9XB	>wB	B�B	H�B	G�B	M�B	I�B	I�B	O�B	N�B	O�B	VB	ffB	l�B	hsB	hsB	hsB	ffB	ffB	iyB	jB	q�B	p�B	l�B	iyB	jB	n�B	s�B	v�B	{�B	|�B	y�B	z�B	}�B	~�B	�B	�B	�+B	�+B	�JB	�bB	�oB	�uB	�{B	�{B	�uB	�{B	��B	��B	��B	�{B	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�-B	�-B	�3B	�3B	�9B	�9B	�?B	�RB	�dB	�qB	�jB	�jB	�wB	�}B	��B	B	B	B	B	ÖB	ÖB	ĜB	ĜB	ŢB	ǮB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�B	�B	�B	�#B	�)B	�/B	�/B	�;B	�BB	�HB	�TB	�TB	�ZB	�ZB	�B	�JB
�B
�B
_B
(�B
2B
7�B
>(B
@4B
FYB
LB
Q�B
X�B
[qB
_pB
c�B
jKB
o5B
utB
y�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B
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
�OB
��B
�B
�pB
�vB�B�B1BCBT�BkBxZBxZB|rB��B�B�B�qB��B�:B�kBڡB��B�!B�B�B�B!B4BBB&eB'kB5B$ZB$ZB$[B*B"NB
�B�B	�B�B�jB�@B��BکB��B��BrBB\�B_�BLbBB
�+B
�pB
�B
��B
�B
��B
y�B
`�B
M�B
=$B
'�B
'B
�B
 �B	�sB	��B	ˇB	�vB	�]B	�9B	�	B	��B	��B	d(B	L�B	9+B	-�B	�B	vB	XB	4B	�B�B�B��B˧B�}B�fB�GB�#B�B�<B�NB�BB�*B��B��B��B��B�BņB�PB�B�KBʥBɠBˬB�FB��B�.B�BƏB��BͺB��BˮBĄB�YB�BBǗB��B��B�B�B��B��BǘB�7B�iB�(BdOBM�BJ�BJ�BO�BR�BQ�BE�BI�BT�BO�BT�BJ�B0B%�B"�B �B�B�B�B �B!�B�B �B�B�B�B�B�B �B�B!�B!�B"�B#�B$�B)�B!�B!�B �B �B#�B#�B"�B"�B#�B$�B$�B'�B)�B,B20B>xBXBZBVBXBVBXB`@Be_BcSBbMBT�BWBU BR�BXBYBXBWBVBQ�BL�BO�BVBP�BS�B\+B]2BZBYBZ BecBy�Bz�B�B�B�B�B�B�B�B�B�B�B�B�B�B�,B�B�B�B�	B}�B}�B�B�B�'B�@B�dB�vB�vB��B��B��B��B��B��B��B��B��B��B��B�B�&B�9B�]BÓBǬBǬB��B��B��B��B��B��B�CB�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B�B��B	B	$B	B��B��B��B	B	<B	UB	�B	"�B	&�B	'�B	(�B	*�B	+�B	-B	/B	*�B	$�B	%�B	%�B	&�B	-B	.B	3)B	7AB	=eB	<_B	B�B	>kB	>kB	D�B	C�B	D�B	J�B	[B	a9B	]"B	]"B	]"B	[B	[B	^(B	_.B	fXB	eRB	a:B	^(B	_.B	cGB	hdB	kwB	p�B	q�B	n�B	o�B	r�B	s�B	v�B	y�B	{�B	{�B	��B	�B	�B	� B	�&B	�&B	�!B	�&B	�>B	�8B	�2B	�'B	�!B	�-B	�3B	�EB	�KB	�EB	�KB	�KB	�KB	�cB	�vB	��B	��B	��B	��B	�|B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	� B	�%B	�1B	�7B	�7B	�7B	�7B	�>B	�>B	�DB	�DB	�JB	�VB	�\B	�bB	�bB	�uB	�{B	ÁB	ÁB	ÁB	ōB	ǙB	ȟB	ɥB	ʫB	ʫB	˱B	;B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� G�O�B	�KB	��B	�nB
]B
B
/B
&�B
,pB
2�B
4�B
:�B
@�B
FqB
M3B
PB
TB
X'B
^�B
c�B
jB
ny111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.52 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9997(+/-0.0001), vertically averaged dS =-0.011(+/-0.004) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144232022020411442320220204114423  AO  ARCAADJP                                                                    20200618141410    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200618141410  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200618141410  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114423  IP                  G�O�G�O�G�O�                